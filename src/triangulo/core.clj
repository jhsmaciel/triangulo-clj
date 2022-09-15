(ns triangulo.core
  (:require [clojure.math :refer [sqrt pow acos to-degrees round]]))

(defn calc-perimetro
  "Calcula o perimetro do triangulo, dado A B e C"
  [a b c]
  (+ a b c))

(defn calc-radianos
  "Calcula radiano ∠A, dado A B C."
  [a b c]
  (-> (pow b 2)
      (+ (pow c 2))
      (- (pow a 2))
      (/ (* b c 2))
      acos))

(defn calc-angulo
  "Calcula o ângulo ∠A, dado A B C."
  [a b c]
  (-> (pow b 2)
      (+ (pow c 2))
      (- (pow a 2))
      (/ (* 2 b c))
      acos
      to-degrees))

(defn calc-area
  "Calcula a área de um triângulo usando a formula de Heron."
  [a b c]
  (let [s (/ (calc-perimetro a b c) 2)
        s-a (- s a)
        s-b (- s b)
        s-c (- s c)]
    (-> s
        (* s-a)
        (* s-b)
        (* s-c)
        sqrt)))

(defn calc-altura
  "Calcula altura de A, dado a AREA."
  [a area]
  (-> 2
      (* area)
      (/ a)))

(defn equilateral?
  "TODO: Verifica se o triangulo é equilateral"
  [a b c]
  (== a b c))

(defn isosceles?
  "Verifica se pelo menos dois lados sao iguais."
  [a b c]
  (->> [(== a b) (== b c) (== a c)]
       (some true?)
       boolean))

(defn escaleno?
  "Verifica se os lados dos triangulos sao diferentes entre si."
  [a b c]
  (not (isosceles? a b c)))

(defn- calcula-angulos-e-compara
  "Função interna que calcula os três ângulos do triângulo faz uma comparação desejada com o ângulos e retorna true se alguma daquelas condições satisfazer"
  [a b c funcao-comparacao funcao-que-decide]
  (->> [(calc-angulo a b c) (calc-angulo b c a) (calc-angulo c a b)]
       (map funcao-comparacao)
       (funcao-que-decide true?)
       boolean))

(defn- angulo-igual-que-90?
  "Verifica se o ângulo indicado é igual a 90 (arredondando)"
  [angulo]
  (= (round angulo) 90))

(defn- angulo-maior-que-90?
  "Verifica se o ângulo indicado é maior que 90"
  [angulo]
  (> (round angulo) 90))

(defn- angulo-menor-que-90?
  "Verifica se o ângulo indicado é menor que 90"
  [angulo]
  (< (round angulo) 90))

(defn retangulo?
  "Verifica se é um triangulo retangulo, cujos angulos são iguais a 90o.
  O resultado não é exato, dado que cada angulo é arredondado utilizando clojure.math/round."
  [a b c]
  (calcula-angulos-e-compara a b c angulo-igual-que-90? some))

(defn obtuso?
  "Verifica se o triangulo é obtuso, tendo algum angulo >90o."
  [a b c]
  (calcula-angulos-e-compara a b c angulo-maior-que-90? some))

(defn agudo?
  "Verifica se o triangulo é obtuso, tendo algum angulo >90o."
  [a b c]
  (calcula-angulos-e-compara a b c angulo-menor-que-90? every?))

(defn gerar-dados-completos
  [a b c]
  (let [area (calc-area a b c)]
    {:lados       [a b c]
     :retagulo    (retangulo? a b c)
     :obtuso      (obtuso? a b c)
     :agudo       (agudo? a b c)
     :escaleno    (escaleno? a b c)
     :isosceles   (isosceles? a b c)
     :equilateral (equilateral? a b c)
     :area        area
     :altura      [(calc-altura a area)
                   (calc-altura b area)
                   (calc-altura c area)]
     :angulos     [(calc-angulo a b c)
                   (calc-angulo b c a)
                   (calc-angulo c a b)]}))

(comment
  (require 'clojure.pprint)
  (escaleno? 60 51.96152 30)
  (retangulo? 60 51.96152 30)
  (clojure.pprint/pprint (gerar-dados-completos 30 20 44))
  (clojure.pprint/pprint (gerar-dados-completos 60 51.96152 30))
  (clojure.pprint/pprint (gerar-dados-completos 15.14741 28.08887 30))
  )
