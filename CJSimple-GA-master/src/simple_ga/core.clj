(ns simple-ga.core
  "Clojure simple genetic algorithm. Usage:
    lein run [-- fitness-function]

   Champlain College
   CSI-380 Spring 2019
   Christopher James Bleau"
  (:gen-class))

(require '[simple-ga.utils :as utils])
(require '[simple-ga.fitness-functions :as ff])

(defn mutate-genome
    "Produce a mutated genome by flipping each bit with mutation-rate probability."
    [genome mutation-rate]
    ;; Flips a coin based with probability of (mutation-rate) then flips the bits if it mutates
    (for [x genome] (if (utils/coin-toss? mutation-rate) (utils/flip-bit x) x)))
         
(defn crossover
    "Perform single-point crossover on the two genomes.
    
    A single genome is returned chosen from the two resulting genomes with equal
    probability.
    
    See: https://en.wikipedia.org/wiki/Crossover_(genetic_algorithm)#Single-point_crossover
    "
    [genome1 genome2]
    ;; chose a single point for single point crossover
    (let [cross (rand-int (count genome1))]
    ;;create each of the child options based off of the crossover point 
    (let [kid1 (conj (subvec genome1 0 cross) (subvec genome2 cross ))]
    (let [kid2 (conj (subvec genome2 0 cross) (subvec genome1 cross))]
    ;; randomly chose a child and return it.
    (if (utils/coin-toss?) (flatten kid1) (flatten kid2))))))

;;HELPER FUNCTION
(defn new-genome
        ;; takes in a genome and returns a new individual
        [ngenome]
        {:genome (vec ngenome)}
        )
            
(defn reproduce
    "Create the next generation from the given parents using the relevant params.
    
    
    The new generation contains a total of (:population-size params) individuals
    including the unmodified parents (this is known as elitism).
    
    The remainder of the individuals are generated as follows:
        with probability (:crossover-rate params) two parents are chosen at random
        and crossed over and the resulting genome is mutated with mutation rate 
        (:mutation-rate params)
           otherwise
        with probability (- 1 (:crossover-rate params)) a single parent is chosen
        at random and its genome is mutated with mutation rate (:mutation-rate params)
    "
    [parents params]
    ;;creates a new generation kids that includes the suriving fit parents from the old generation.
    ;;loops until it creates population size individuals that have been created by one of the two methods
    ;; then conjoins them to a new final generation
        (loop [x (- (:population-size params) (:num-parents params )) kids (vec parents)]
            (if (> x 0)
                (recur (dec x)
                    (conj kids 
                         (if 
                            ;;flips a coin based on the crossover rate. If true crossover then mutate if false then just mutate with chance mutation rate
                            (utils/coin-toss? (:crossover-rate params)) 
                            (new-genome (mutate-genome (crossover (:genome (rand-nth parents))(:genome (rand-nth parents)))(:mutation-rate params)))
                            (new-genome (mutate-genome (:genome (rand-nth parents)) (:mutation-rate params))))))kids)))

(defn generate-individual
    "Generate a single individual.
    
    Each individual is a map with key :genome associated with a genome-size length
    vector of bits (0s and 1s)
    "
    [genome-size]
    {:genome (mapv (fn [_] (rand-int 2)) 
                (range genome-size))})
    
(defn generate-population
    "Generate the population.
    
    The population is a collection of (:population-size params) individuals,
    where each individual is a map with key :genome associated with a
    (:genome-size params) length vector of bits (0s and 1s)
    "
    [params]
    ;;Generates a population of :population-size with genomes the size of :genome-size
    (for [x (range (:population-size params))]
    (conj(generate-individual(:genome-size params)))))


(defn select-parents
    "Select and return the num-parents most fit individuals in the population.
    
    This is known truncation selection.
    "
    [population num-parents]
    ;; sorts population by fitness and takes the (:num-parents) top fitness scores. 
    (take num-parents (sort-by :fitness > population)))
    
(defn evaluate-individual
    "Evaluate a given individual on the specified fitness function.
    
    Returns a copy of the individual with :fitness key set to its fitness.
    
    This should evaluate the individual and set/update its fitness even if it already has a fitness value.
    "
    [individual fitness-function]
    (assoc individual :fitness (fitness-function (:genome individual))))
    
(defn evaluate-population
    "Evaluate the entire population in parallel.
    
    Returns a copy of the population with the :fitness key of each individual set to its fitness.
    
    This should evaluate all individuals in parallel and set/update their fitness (even ones that already have a fitness value).
    "
    ;;Using the pmap tip, concurrently maps all the individuals with a new key value pair of a fitness score :fitness
    ;;This is done by calling evaluate-individual
    [population fitness-function]
    (pmap (fn [x] (evaluate-individual x fitness-function)) population))
    

(defn run-generation
    "Run a single generation of a genetic algorithm.
    
    Performs the following steps:
        (1) evaluates the current population
        (2) selects parents
        (3) builds and returns the next generation by reproducing the parents.
    "

    ;;Does the three steps above effectively by calling reproduce, select-parents, and evaluate population on the next generation.
    ;; This prepares a new generation to be called again
    [population params]
    (let [generation
        (reproduce
            (select-parents
                (evaluate-population population (:fitness-function params)) 
            (:num-parents params)
            )
        params)] generation)

)

         
(defn evolve
    "Run a genetic algorithm with the given params and return the best individual in the final generation."
    [params]
    (loop [generations-remaining (:num-generations params)
           current-population (generate-population params)]
           (if (zero? generations-remaining)
               ;; if no more generations, still need to evaluate the final population
               ;; in order to determine the best individual
               (let [evaluated-population (evaluate-population current-population (:fitness-function params))]
                    (apply max-key :fitness  evaluated-population))
               ;; otherwise move on to next generation
               (do  (println "Generation" (+ 1 (- (:num-generations params) generations-remaining)))
                    (recur (dec generations-remaining)
                           (run-generation current-population params))))))
         

(defn -main
  "Main function, run GA on max-ones problem or specified fitness function.
  
  Prints best indivdual to screen.
  "
  [& args]
  (if (> (count args) 1)
      (println "Usage:\n\tlein run [-- fitness-function]")
      (let [params {:genome-size 16
                    :population-size 100
                    :num-generations 50
                    :num-parents 5
                    :crossover-rate 0.75
                    :mutation-rate 0.1
                    :fitness-function (if (empty? args) 
                                        ff/max-ones ;; default to max-ones fitness function
                                        (resolve (symbol "simple-ga.fitness-functions" (first args))))
                   }]
       (if (:fitness-function params)
           ;; if fitness function is defined
           (println (evolve params))
           ;; otherwise invalid fitness function
           (println "unknown fitness function:" (first args)))
       (shutdown-agents)))) ;; needed in case pmap is used, otherwise program won't terminate
