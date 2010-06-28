(ns impojure.class.io)

(defn read-u1 [in]
  (.readUnsignedByte in))

(defn read-u2 [in]
  (.readUnsignedShort in))

(defn read-u4 [in]
  (let [signed (.readInt in)]
    (if (neg? signed)
      (+ signed 4294967296)
      signed)))

(defn read-u8 [in]
  (let [high (read-u4 in)
	low (read-u4 in)]
    (+ (bit-shift-left high 32) low)))

(defn read-int [in]
  (.readInt in))

(defn read-float [in]
  (.readFloat in))

(defn read-long [in]
  (.readLong in))

(defn read-double [in]
  (.readDouble in))

(defn read-utf8 [in]
  (.readUTF in))