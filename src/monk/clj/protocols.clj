(ns monk.clj.protocols)

(defprotocol IVal (-val [_]))
(defprotocol IMap (-map [_]))
(defprotocol IVec (-vec [_]))
(defprotocol IForm (-form [_]))
(defprotocol IStep (-step [_]))
(defprotocol ILens (-lens [_]))
(defprotocol ISend (-send [_ f]))
(defprotocol ICons (-cons [_ s]))

(defn method-sym->protocol-sym
  [method-name]
  (get {'-val `IVal
        '-map `IMap
        '-vec `IVec
        '-form `IForm
        '-step `IStep
        '-lens `ILens
        '-send `ISend
        '-cons `ICons}
       method-name))

(def METHODS
  '{:val {:method-sym -val, :protocol-sym IVal},
    :map {:method-sym -map, :protocol-sym IMap},
    :vec {:method-sym -vec, :protocol-sym IVec},
    :form {:method-sym -form, :protocol-sym IForm},
    :step {:method-sym -step, :protocol-sym IStep},
    :lens {:method-sym -lens, :protocol-sym ILens},
    :send {:method-sym -send, :protocol-sym ISend},
    :cons {:method-sym -cons, :protocol-sym ICons}})
