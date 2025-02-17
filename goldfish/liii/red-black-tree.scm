;
; Copyright (C) 2024 The Goldfish Scheme Authors
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
; http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
; License for the specific language governing permissions and limitations
; under the License.
;

(define-library (liii red-black-tree)
(import (liii error))
(export make-red-black-tree red-black-tree?
        red-black-tree-empty?
        red-black-tree-find red-black-tree-insert
        red-black-tree-delete)
(begin

(define null '())

(define-record-type :red-black-tree
  (%make-red-black-tree root compare)
  red-black-tree?
  (root red-black-tree-root)
  (compare red-black-tree-compare))

(define-record-type :node
  (make-node color key val left right)
  node?
  (color node-color)
  (key node-key)
  (val node-val)
  (left node-left)
  (right node-right))

(define (make-red-black-tree compare)
  (when (not (procedure? compare))
    (type-error "make-red-black-tree: compare must be a compare procedure which takes 2 elements and returns 'eq,'lt, or 'gt"))
  (%make-red-black-tree null compare))

(define (red-black-tree-empty? t)
  (when (not (red-black-tree? t))
    (type-error "red-black-tree-empty: t must be a red black tree"))
  (null? (red-black-tree-root t)))

(define (red-black-tree-find t key)
  (when (not (red-black-tree? t))
    (type-error "red-black-tree-find: t must be a red black tree"))
  (define root (red-black-tree-root t))
  (define compare (red-black-tree-compare t))

  (define (find node)
    (if (null? node)
        #f
        (case (compare key (node-key node))
          ((eq) (node-val node))
          ((lt) (find (node-left node)))
          ((gt) (find (node-right node))))))

  (find root))

(define (red-node? node)
  (and (node? node) 
       (eq? (node-color node) 'red)))

(define (black-node? node)
  (or (null? node) 
      (eq? (node-color node) 'black)))

(define (rotate-left node)
  (define x (node-left node))
  (define y (node-right node))
  (define z (node-left y))
  (define t (node-right y))

  (make-node (node-color node) (node-key y) (node-val y) 
    (make-node 'red (node-key node) (node-val node) x z)
    t))

(define (rotate-right node)
  (define x (node-left node))
  (define y (node-left x))
  (define z (node-right x))
  (define t (node-right node))

  (make-node (node-color node) (node-key x) (node-val x)
    y
    (make-node 'red (node-key node) (node-val node) z t)))

(define (flip-colors node)
  (define (flip c)
    (case c
      ((red) 'black)
      ((black) 'red)))

  (define (flip-node node)
    (make-node (flip (node-color node)) (node-key node) 
               (node-val node) (node-left node) (node-right node)))

  (make-node (flip (node-color node)) (node-key node) (node-val node) 
             (flip-node (node-left node)) (flip-node (node-right node))))


(define (balance node)
  (cond 
    ((and (red-node? (node-right node)) (black-node? (node-left node))) 
      (rotate-left node))
    ((and (red-node? (node-left node)) (red-node? (node-left (node-left node))))
      (rotate-right node))
    ((and (red-node? (node-left node)) (red-node? (node-right node)))
      (flip-colors node))
    (else node)))

(define (red-black-tree-insert t key val)
  (when (not (red-black-tree? t))
    (type-error "red-black-tree-insert: t must be a red black tree"))
  (define root (red-black-tree-root t))
  (define compare (red-black-tree-compare t))

  (define (insert node)
    (if (null? node) 
      (make-node 'red key val null null)
      (balance (case (compare key (node-key node))
                     ((eq) (make-node (node-color node) key val 
                                      (node-left node) (node-right node)))
                     ((lt) (make-node (node-color node) (node-key node) (node-val node) 
                                      (insert (node-left node)) (node-right node)))
                     ((gt) (make-node (node-color node) (node-key node) (node-val node) 
                                      (node-left node) (insert (node-right node))))))))
  
  (let ((new-root (insert root)))
       (%make-red-black-tree (make-node 'black (node-key new-root) (node-val new-root)
                                         (node-left new-root) (node-right new-root))
                             compare)))

(define (move-red-left node)
  (define flipped (flip-colors node))
  (if (red-node? (node-left (node-right flipped))) 
      (rotate-left (rotate-right flipped)) 
      flipped))

(define (move-red-right node)
  (define flipped (flip-colors node))
  (if (red-node? (node-left (node-left flipped)))
      (rotate-right flipped) 
      flipped))

(define (red-black-tree-delete t key)
  (when (not (red-black-tree? t))
    (type-error "red-black-tree-delete: t must be a red black tree"))
  (define root (red-black-tree-root t))
  (define compare (red-black-tree-compare t))

  (define (delete-min node)
    (if (null? (node-left node)) null
        (let ((node1 (if (and (black-node? (node-left node)) (black-node? (node-left (node-left node)))) 
                          (move-red-left node) 
                          node)))
             (balance (make-node (node-color node1) (node-key node1) (node-val node1) 
                                 (delete-min (node-left node1)) (node-right node1))))))

  (define (min-node node)
    (if (null? (node-left node)) node (min-node (node-left node))))
  
  (define (delete-left node)
    (when (null? (node-left node))
        (error 'key-not-found "red-black-tree-delete: key not found"))
    (let ((node1 (if (and (black-node? (node-left node)) (black-node? (node-left (node-left node))))
                     (move-red-left node) 
                     node)))
         (balance (make-node (node-color node1) (node-key node1) (node-val node1) 
                  (delete (node-left node1)) (node-right node1)))))

  (define (delete-right node)
    (define node1 (if (red-node? (node-left node)) 
                      (rotate-right node) 
                      node))
    (if (and (eq? (compare key (node-key node1)) 'eq) (null? (node-right node1))) 
        null
        (begin
          (when (null? (node-right node1))
            (error 'key-not-found "red-black-tree-delete: key not found"))
          (let ((node2 (if (and (black-node? (node-right node1)) (black-node? (node-left (node-right node1)))) 
                           (move-red-right node1)
                           node1)))
               (if (eq? (compare key (node-key node2)) 'eq)
                   (let ((minimum (min-node (node-right node2))))
                        (balance (make-node (node-color node2) (node-key minimum) (node-val minimum)
                                 (node-left node2) (delete-min (node-right node2)))))
                   (balance (make-node (node-color node2) (node-key node2) (node-val node2)
                            (node-left node2) (delete (node-right node2)))))))))

  (define (delete node)
    (if (eq? (compare key (node-key node)) 'lt) 
        (delete-left node) 
        (delete-right node)))

  (define (delete-root)
    (when (null? root)
      (error 'key-not-found "red-black-tree-delete: key not found"))
    (let* ((root1 (if (and (black-node? (node-left root)) (black-node? (node-right root)))
                      (make-node 'red (node-key root) (node-val root)
                                 (node-left root) (node-right root))
                      root))
           (root2 (delete root1))
           (root3 (if (null? root2)
                      root2 
                      (make-node 'black (node-key root2) (node-key root2)
                                 (node-left root2) (node-right root2)))))
          (%make-red-black-tree root3 compare)))
  (delete-root))

) ; end of begin
) ; end of library