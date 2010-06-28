(ns impojure.class.constants)

(def constant-type-to-tag
     [nil :text nil :int :float :long :double :class :string
      :field :method :imethod :member])

(def base-types
     {\B :byte, \C :char, \D :double, \F :float,
      \I :int,  \J :long, \S :short,  \V :void, \Z :boolean})

(def unknown-access-flags
     (into {} (for [[i bit] (take 16 (iterate (fn [[i bit]]
                                                [(inc i) (* bit 2)])
                                              [0 1]))]
                [bit (keyword (format "unknown%d" i))])))

(def class-access-flags
     (assoc unknown-access-flags
       0x0001 :public
       0x0010 :final
       0x0020 :super
       0x0200 :interface
       0x0400 :abstract))

(def field-access-flags
     (assoc unknown-access-flags
       0x0001 :public
       0x0002 :private
       0x0004 :protected
       0x0008 :static
       0x0010 :final
       0x0040 :volatile
       0x0080 :transient))

(def method-access-flags
     (assoc unknown-access-flags
       0x0001 :public
       0x0002 :private
       0x0004 :protected
       0x0008 :static
       0x0010 :final
       0x0020 :synchronized
       0x0100 :native
       0x0400 :abstract
       0x0800 :strict))

(def unknown-access-flag?
     (set (vals unknown-access-flags)))

(def class-access-flag?
     (set (vals class-access-flags)))

(def field-access-flag?
     (set (vals field-access-flags)))

(def method-access-flag?
     (set (vals method-access-flags)))

(derive :impojure/class :impojure/class-field-method)

(derive :impojure/field :impojure/class-field-method)

(derive :impojure/method :impojure/class-field-method)
