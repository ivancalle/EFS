package fuzzyrules.genetics

import collection.mutable.ArrayBuffer
import scala.util.Random
import fuzzyrules.utils.KnowledgeBase
import fuzzyrules.utils.TrainPoint

class IndividualRS extends IndividualInterface{
	var chromosomesBoolean: Array[Boolean] = Array[Boolean]();
	def this(chromosomes: Array[Boolean]) = {
		this();
		var temp = ArrayBuffer[Any]()
		for( chromosome <- chromosomes) {
			temp += chromosome
		}
		this.chromosomes = temp.toArray;
		this.chromosomesBoolean = chromosomes;
	}

	def this(chromosomes: Array[Any], fitness: Double) = {
		this();
		this.chromosomes = chromosomes;
		var temp = ArrayBuffer[Boolean]()
		for( chromosome <- chromosomes) {
			temp += chromosome.asInstanceOf[Boolean]
		}
		this.chromosomesBoolean = temp.toArray;
		this.fitness = fitness
	}

	def this(size: Int) = {
		this();
		val rand = new Random()
		var chromosomes = ArrayBuffer[Any]();
		var chromosomesB = ArrayBuffer[Boolean]();
		for( i <- 0 until size) {
			var bool = rand.nextBoolean()
			chromosomes += bool
			chromosomesB += bool
		}
		this.chromosomes = chromosomes.toArray;
		this.chromosomesBoolean = chromosomesB.toArray;
	}

	def updateFitness(trainSet: Array[TrainPoint], kBase :KnowledgeBase) = {
		var success = 0;
		var total = 0;
		for(example <- trainSet) {
			var classIndex = this.classify(example.values, kBase);
			if(classIndex == example.consecuence){
				success = success + 1;
			}
			total = total + 1;
		}
		
		this.fitness = (1.0*success)/total;;
	}

	def classify(point: Array[Double], kBase :KnowledgeBase): Int = {
		var maxDegree = 0.0;
		var classIndex = 0
		var deggre = 0.0;
		for((rule, i)  <- kBase.getRules().zipWithIndex) {
			if(this.chromosomesBoolean(i)){
				deggre = rule.getMatchingDegree(point, kBase.getAttributes());
				if(deggre > maxDegree){
					maxDegree = deggre;
					classIndex = rule.consecuence;
				}
			}
		}

		classIndex;
	}

	def updateKBase(kBase :KnowledgeBase)  = {
		var list = ArrayBuffer[Int]();
		for(i  <- 0 until kBase.size()) {
			if(!this.chromosomesBoolean(i)){
				list += i;
			}
		}
		kBase.removeRuleList(list.toArray);
	}

	def createHash(): String = {
		this.chromosomes.mkString(",")
	}

	override def copy(): IndividualInterface = {
		new IndividualRS(this.chromosomes.clone(), this.fitness)
	}
}