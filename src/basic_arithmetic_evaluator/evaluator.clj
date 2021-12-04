(ns basic-arithmetic-evaluator.evaluator
  (:gen-class))

(defn parse-digit
  "Takes a character as input and returns its literal matching digit.
  If not in a digit range, returns nil."
  [c]
  (let [n (- (int c) (int \0))]
    (when (and (>= n 0) (<= n 9))
      n)))

(defn tokenize 
  "Takes a sequence of character as input and returns a sequence of token.
  If the input is falsy, returns nil.
  A non recognized character is ignored."
  [expr]
  (when (seq expr)
    (let [[x & xs] expr]
      (case x
        \+ (cons {:token :plus} (tokenize xs))
        \* (cons {:token :mult} (tokenize xs))
        (if-let [x (parse-digit x)]
          (cons {:token x} (tokenize xs))
          (recur xs))))))

(defn reduce-tokens
  "Transform a sequence of token into an AST node.
  Operator precedence is ignored."
  [tokens]
  (when (seq tokens)
    (let [x {:type :literal :value (:token (first tokens))}
          op (second tokens)
          xs (rest (rest tokens))]
      (if (nil? op)
        x
        {:type (:token op)
         :left x
         :right (reduce-tokens xs)}))))
  
(defn parse
  "Takes an operator precedence, consumed token sequence and remaining
  token sequence to consume.
  If the current token is a literal or an operator from similar or
  bigger precedence, adds the current token in the consume sequence
  and continues on next token.
  If the current token is an operator with a smaller precedence,
  creates AST node for consumed and current token and continues."
  [prec consumed tokens]
  (if (empty? tokens)
    (reduce-tokens consumed)
    (let [x (first tokens)
          xs (rest tokens)
          op-case (fn [opPrec op]
                    (if (> prec opPrec)
                      {:type op
                       :left (reduce-tokens consumed)
                       :right (parse opPrec [] xs)}
                      (parse opPrec (conj consumed x) xs)))]
      (case (:token x)
        :plus (op-case 1 :plus)
        :mult (op-case 2 :mult)
        (recur prec (conj consumed x) xs)))))

(defn eval-expr
  "Takes an AST node as input, evaluates it and returns its value.
  If a child node evaluation is falsy, replaces it by the operation
  neutral value.
  If node is unknown, returns nil."
  [node]
  (let [eval-binary #(%1 (or (eval-expr (:left node)) %2)
                         (or (eval-expr (:right node)) %2))]
    (case (:type node)
      :plus (eval-binary + 0)
      :mult (eval-binary * 1)
      :literal (:value node)
      nil)))

(defn calc
  "Takes a basic arithmetic expression as sequence of characters,
  evaluates it and returns its result. If the expression is empty or
  ill-formed, returns nil."
  [expr]
  (->> expr
       tokenize
       (parse 1 [])
       eval-expr))
