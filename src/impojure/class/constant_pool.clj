(ns impojure.class.constant-pool
  (:use (impojure.class io constants)))

(declare read-constant-pool read-constant-pool-info
         lookup-class lookup-name lookup-descriptor)

(declare flatten-constant-pool flatten-string-info flatten-class-info
         flatten-member-info flatten-field-method-imethod-info)

(declare parse-class-name parse-types parse-descriptor)

(def *last-constant-type*)

(defn read-constant-pool [in]
  (let [constant-count (read-u2 in)]
    (assert (pos? constant-count))
    (binding [*last-constant-type* nil]
      (into [nil] (for [_ (range (dec constant-count))]
		    (read-constant-pool-info in))))))

(defn read-constant-pool-info [in]
  (if (#{:long :double} *last-constant-type*)
    (do (set! *last-constant-type* nil)
	[:unusable nil])
    (let [tag (get constant-type-to-tag (read-u1 in))]
      (assert (not (nil? tag)))
      (set! *last-constant-type* tag)
      [tag (condp = tag
	     :class   (read-u2 in)
	     :field   [(read-u2 in) (read-u2 in)]
	     :method  [(read-u2 in) (read-u2 in)]
	     :imethod [(read-u2 in) (read-u2 in)]
	     :string  (read-u2 in)
	     :int     (read-int in)
	     :float   (read-float in)
	     :long    (read-long in)
	     :double  (read-double in)
	     :member  [(read-u2 in) (read-u2 in)]
	     :text    (.intern (read-utf8 in)))])))

(defn lookup-class [constant-pool index]
  (let [[tag value] (constant-pool index)]
    (assert (= tag :class))
    (parse-class-name value)))

(defn lookup-name [constant-pool index]
  (let [[tag value] (constant-pool index)]
    (assert (= tag :text))
    value))

(defn lookup-descriptor [constant-pool index]
  (let [[tag value] (constant-pool index)]
    (assert (= tag :text))
    (parse-descriptor value)))

(defn flatten-constant-pool [constant-pool]
  (-> constant-pool
      flatten-string-info
      flatten-class-info
      flatten-member-info
      flatten-field-method-imethod-info))

(defn flatten-string-info [constant-pool]
  (vec (for [[tag index :as info] constant-pool]
	 (if (= tag :string)
	   (let [[tag2 text] (constant-pool index)]
	     (assert (= tag2 :text))
	     [tag text])
	   info))))

(defn flatten-class-info [constant-pool]
  (vec (for [[tag index :as info] constant-pool]
	 (if (= tag :class)
	   (let [[tag2 name] (constant-pool index)]
	     (assert (= tag2 :text))
	     [tag (parse-class-name name)])
	   info))))

(defn flatten-member-info [constant-pool]
  (vec (for [[tag data :as info] constant-pool]
	 (if (= tag :member)
	   (let [[name-index desc-index] data
		 [tag2 name] (constant-pool name-index)
		 [tag3 descriptor] (constant-pool desc-index)]
	     (assert (= tag2 :text))
	     (assert (= tag3 :text))
	     [tag [name (parse-descriptor descriptor)]])
	   info))))

(defn flatten-field-method-imethod-info [constant-pool]
  (vec (for [[tag data :as info] constant-pool]
	 (if (or (= tag :field)
		 (= tag :method)
		 (= tag :imethod))
	   (let [[class-index member-index] data
		 [tag2 class] (constant-pool class-index)
		 [tag3 member] (constant-pool member-index)]
	     (assert (= tag2 :class))
	     (assert (= tag3 :member))
	     [tag [class member]])
	   info))))

(defn parse-class-name [class-name]
  (.replace class-name \/ \.))

(defn parse-types [desc]
  (for [[_ array-dimensions base-type object-type] (re-seq #"(\[*)(?:([BCDFIJSVZ])|L(.*?);)" desc)]
    (let [component-type (cond base-type [:primitive (base-types (first base-type))]
			       object-type [:reference (parse-class-name object-type)])]
      (if (zero? (count array-dimensions))
        component-type
        [:array component-type (count array-dimensions)]))))

(defn parse-descriptor [desc]
  (if-let [[_ param-descs return-desc] (re-find #"^\((.*)\)(.+)$" desc)]
    (let [param-types (parse-types param-descs)
	  return-type (parse-types return-desc)]
      (assert (= (count return-type) 1))
      (assert (not-any? #(= % [:primitive :void]) param-types))
      {:params param-types
       :return (first return-type)})
    (let [type (parse-types desc)]
      (assert (= (count type) 1))
      (assert (not= (first type) [:primitive :void]))
      (first type))))