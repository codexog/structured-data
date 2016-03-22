(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx))) 

(defn spiff [v]
  (+ (get v 2 0) (get v 0 0)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[a b][c b]] rectangle]
    (- c a)))

(defn height [rectangle]
  (let [[[b a][b c]] rectangle]
    (- c a)))

(defn square? [rectangle]
  (if(= (height rectangle) (width rectangle))
    true
    false))

(defn area [rectangle]
  (let [[[x y][z v]] rectangle]
    (* (height rectangle) (width rectangle))))

(defn contains-point? [[[x1 y1] [x2 y2]] [x3 y3]]
  (if (and (<= x1 x3 x2) (<= y1 y3 y2))
    true
    false))

(defn contains-rectangle? [outer inner]
  (let [[x y] inner] 
    (if (and (contains-point? outer x)
             (contains-point? outer y))
      true
      false)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (not (= (author-count book) 1)))

(defn add-author [book new-author]
  (assoc-in book [:authors (count (:authors book))] new-author))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map #(get % 1) collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (distinct a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (apply str (cons (:name author) (if (contains? author :birth-year)
                       (list " ("
                          (:birth-year author)
                          " - "
                          (:death-year author)
                          ")")
                       (list)))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (apply str (cons (:title book) (cons ", written by "  (authors->string (:authors book))))))

(defn books->string [books]
  (cond
    (= (count books) 0) "No books."
    (= (count books) 1) (apply str (cons "1 book. " (concat (interpose ". " (map book->string books)) (list "."))))
    (> (count books) 1) (apply str (concat (cons (count books) (cons " books. " (interpose ". " (map book->string books)))) "."))))

(defn books-by-author [author books]
  (filter #(has-author? % author) books))

(defn author-by-name [name authors]
  (first (filter #(= name (:name %)) authors)))

(defn living-authors [authors]
  (filter #(alive? %) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
