(ns impojure.class.attribute
  (:use (impojure.class io constants constant-pool)))

(declare read-class-attributes read-field-attributes
         read-attributes read-attribute)

(defn read-attributes [structure constant-pool in]
  (let [attr-count (read-u2 in)]
    (into {}
          (for [_ (range attr-count)]
            (let [attr-name (lookup-name constant-pool (read-u2 in))
                  attr-length (read-u4 in)]
              [attr-name (read-attribute structure
                                         attr-name attr-length
                                         constant-pool in)])))))

(defmulti read-attribute
  (fn [structure attribute-name attribute-length constant-pool in]
    [structure attribute-name]))

(defmethod read-attribute :default
  [_ _ attribute-length constant-pool in]
  (vec (for [_ (range attribute-length)]
         (read-u1 in))))

(defmethod read-attribute [:impojure/field "ConstantValue"]
  [_ _ _ constant-pool in]
  (constant-pool (read-u2 in)))

(defn read-exception-table [constant-pool in]
  (let [exception-table-length (read-u2 in)]
    (doall
     (for [_ (range exception-table-length)]
       (let [start-pc (read-u2 in)
             end-pc (read-u2 in)
             handler-pc (read-u2 in)
             catch-type-index (read-u2 in)]
         (assoc {:start-pc start-pc
                 :end-pc end-pc
                 :handler-pc handler-pc}
           :catch-type (if (zero? catch-type-index)
                         nil
                         (lookup-class constant-pool catch-type-index))))))))

(defmethod read-attribute [:impojure/method "Code"]
  [_ _ _ constant-pool in]
  (let [max-stack (read-u2 in)
        max-locals (read-u2 in)
        code-length (read-u4 in)
        code (vec (for [_ (range code-length)]
                    (read-u1 in)))
        exception-table (read-exception-table constant-pool in)
        code-attributes (read-attributes :impojure/code constant-pool in)]
    (assert (pos? (count code)))
    {:max-stack max-stack
     :max-locals max-locals
     :instructions code
     :exception-table exception-table
     :attributes code-attributes}))

(defmethod read-attribute [:impojure/method "Exceptions"]
  [_ _ _ constant-pool in]
  (let [exception-count (read-u2 in)]
    (doall
     (for [_ (range exception-count)]
       (lookup-class constant-pool (read-u2 in))))))

(defmethod read-attribute [:impojure/class-field-method "Synthetic"]
  [_ _ _ constant-pool in]
  true)

(defmethod read-attribute [:impojure/class "SourceFile"]
  [_ _ _ constant-pool in]
  (lookup-name constant-pool (read-u2 in)))

(defmethod read-attribute [:impojure/code "LineNumberTable"]
  [_ _ _ constant-pool in]
  (let [line-number-table-length (read-u2 in)]
    (doall
     (for [_ (range line-number-table-length)]
       (hash-map :start-pc (read-u2 in)
                 :line-number (read-u2 in))))))

(defmethod read-attribute [:impojure/code "LocalVariableTable"]
  [_ _ _ constant-pool in]
  (let [local-variable-table-length (read-u2 in)]
    (doall
     (for [_ (range local-variable-table-length)]
       (hash-map :start-pc (read-u2 in)
                 :length (read-u2 in)
                 :name (lookup-name constant-pool (read-u2 in))
                 :type (lookup-descriptor constant-pool (read-u2 in))
                 :index (read-u2 in))))))

(defmethod read-attribute [:impojure/class-field-method "Deprecated"]
  [_ _ _ constant-pool in]
  true)

#_(defmethod read-attribute [:impojure/class-field-method "RuntimeVisibleAnnotations"]
  [_ _ _ constant-pool in]
  (let [annotation-count (read-u2 in)]
    (doall
     (for [_ (range annotation-count)]
       (let [annotation-type (lookup-descriptor constant-pool (read-u2 in))
             element-value-pair-count(read-u2 in)]
         (assert (zero? element-value-pair-count))
         {:type annotation-type
          :annotations ()})))))
