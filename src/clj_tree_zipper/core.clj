(ns clj-tree-zipper.core
  (:require [clojure.zip :as z]))



;; protocol for records in the tree to implement
(defprotocol ZipTree
  (branch? [node] "Is it possible for node to have children?")
  (node-children [node] "Return children of this node.")
  (make-node [node children] "Makes new node from existing node and children."))

(extend-type Object ZipTree
             (branch? [node] false)
             (make-node [node children] node))

;; tree zipper
(defn tree-zip
  "Make a zipper out of a tree."
  [root]
  (z/zipper branch? node-children make-node root))

;; create out records and implement protocol
(defrecord Directory [name children])

(defrecord File [name])

(extend-protocol ZipTree

  File
  (branch? [node] false)
  (node-children [node] '())
  (make-node [node children] (File. node))

  Directory
  (branch? [node] true)
  (node-children [node] (:children node))
  (make-node [node children] (Directory. (:name node) children))
  )

(def ^:dynamic *root* (tree-zip (Directory. "/" nil)))

(defn- apply-first-and-repeated
  "Apply function f to x, then apply function fr to the chained result n times.
   This is used in breath 1st search of the tree."
  [x f fr n]
  (let [firstvalue (f x)]
    (loop [cnt n
           holder firstvalue]
      (if (= cnt 0)
        holder
        (recur (dec cnt) (fr holder))))))

(defn- find-child-index
  "given a tree, find an immediate child with the name 'name'
   return index starting with 0, -1 return value indicates non-match"
  [tree name]
  (let [children-names (map :name (z/children tree))]
    (.indexOf children-names name)))

(defn- find-child
  "Given tree and index, in a list, return the tree represting the child."
  [tree index]
  (apply-first-and-repeated tree z/down z/right index))

(defn- find-named-child
  "Use find-child to find a child with a given name."
  [tree name]
  (let [index (find-child-index tree name)]
    (if (= -1 index)
      nil
      (find-child tree index))))

(defn find-path
  "path is a list of the form '('/' 'tmp' 'sub'), would find '/tmp/sub'"
  [tree path]
  (if (= path '("/"))
    tree
    (loop [ r-path (rest path)
           r-tree tree]
      (if (or (= '() r-path) (not r-tree))
        r-tree
        (recur (rest r-path)
               (find-named-child r-tree (first r-path)))))))

(defn path-up
  "Returns a list, reverse of which can be used to find the current node from root"
  [tree]
  (let [parent (z/up tree)
        name (:name (z/node tree))]
    (if parent
      (cons name (path-up parent))
      (list name))))

(defn remove-path
  "returns nil if unable to remove (can't remove root, path does not exists)
   returns new tree if removal worked"
  [tree path]
  (if-not (or tree (= path '("/")))
    nil
    (let [remove-me (find-path tree path)]
      (if-not remove-me
        nil
        (tree-zip (z/root (z/remove remove-me)))))))

(defn add-at-path
  "dir-path : '( '/' <elem str> <elem str> ....)
   returns nil if not possible to add, meaning intermediate path elem missing
   returns a new tree if addition was made
   returns the original tree if the path already exists"
  [tree dir-path element]
  (let [parent-node (find-path tree dir-path)]
    (if-not parent-node
      nil
      (tree-zip (z/root (z/insert-child
                         parent-node
                         element))))))
