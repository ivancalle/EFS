package fuzzyrules.genetics

import collection.mutable.ArrayBuffer
import scala.util.Random
import fuzzyrules.utils._
import scala.collection.immutable.Map

class Population{
	var kBase: KnowledgeBase = null;
	var population: ArrayBuffer[IndividualInterface] = null;
	var trainSet :Array[TrainPoint] = null;
	var nEvals: Int = -1;
	var threshold: Double = -1.0;
	var memory: Map[String, Double] = Map[String, Double]()

	def this(kBase: KnowledgeBase, trainSet :Array[TrainPoint], nEvals: Int) = {
		this();
		this.kBase = kBase;
		this.trainSet = trainSet;
		this.nEvals = nEvals;
		this.population = ArrayBuffer[IndividualInterface]();
		this.threshold = this.kBase.size()/4.0;

	}

	def initialize(ini: Array[IndividualInterface]) = {
		this.population.clear()
		for( individual <- ini) {
			this.population += individual;
		}

	}

	def updateFitness(): Boolean = {
		var newIndividuals = false;
		for ( individual <- this.population){
			if (!individual.isEvaluate()){
				newIndividuals = true;
				var individualHash = individual.createHash() 
				if((this.memory get individualHash) == None){
					individual.updateFitness(this.trainSet, this.kBase);
					this.memory += (individualHash -> individual.getFitness());
				}else{
					individual.setFitness(this.memory(individualHash))
				}
				this.nEvals -= 1;
			}
		}
		newIndividuals
	}

	def cross() = {
		var couples = Array.fill[Int](this.population.size)(0);
		for( i <- 0 until couples.size) {
			couples(i) = i;
		}

		val rand = new Random();
		for( i <- 0 until couples.size) {
			var j = rand.nextInt(couples.size);
			var aux = couples(j);
			couples(j) = couples(i);
			couples(i) = aux;
		}

		for( i <- 0 until (couples.size-2) by 2) {
			var mom = this.population(couples(i));
			var dad = this.population(couples(i+1));
			if(mom.hamming(dad)/2.0 > this.threshold){
				var tuple = mom.HUX(dad)
				this.population += tuple._1
				this.population += tuple._2
				//OnePoint(mom,dad);
				//xPC_BLX(mom,dad);
			}
		}
		
	}

	def select(n:Int) = {
		this.sort()
		this.population.remove(n, this.population.length- n)
	}

	def getFirsts(n: Int): Array[IndividualInterface] = {
		this.sort();
		var ret = Array[IndividualInterface]();
		if(n >= this.population.length){
			ret = this.population.toArray;
		}else{
			var aux = ArrayBuffer[IndividualInterface]();
			for( i <- 0 until n) {
				aux += this.population(i)
			}
			ret = aux.toArray
		}
		ret
	}

	def sort(){
		this.population = this.population.sortWith(_.getFitness() > _.getFitness());
	}

}