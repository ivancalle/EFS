package fuzzyrules.genetics

import collection.mutable.ArrayBuffer
import scala.util.Random
import fuzzyrules.utils.KnowledgeBase
import fuzzyrules.utils.TrainPoint

abstract class IndividualInterface{
	var chromosomes: Array[Any] = Array[Any]();
	protected var fitness: Double = -1.0;

	def this(chromosomes: Array[Any], fitness: Double) = {
		this();
		this.chromosomes = chromosomes;
		this.fitness = fitness
	}

	def setFitness(fitness: Double) = { this.fitness = fitness }
	def getFitness(): Double = { return this.fitness }
	def isEvaluate(): Boolean = {return this.fitness >= 0.0}

	def hamming(individual: IndividualInterface): Int = {
		var dist = 0;
		for( (chromo, i) <- individual.chromosomes.zipWithIndex) {
			if(chromo != this.chromosomes(i)){
				dist += 1;
			}
		}
		dist
	}

	def differ(individual: IndividualInterface): Array[Int] = {
		var differ = ArrayBuffer[Int]();
		for( (chromo, i) <- individual.chromosomes.zipWithIndex) {
			if(chromo != this.chromosomes(i)){
				differ += i;
			}
		}
		differ.toArray
	}

	def compare(individual: IndividualInterface): Int = {
		if(individual.fitness < this.fitness){
			return -1;
		}else if(individual.fitness > this.fitness){
			return 1;
		}
		return 0;
	}

	def HUX(partner: IndividualInterface): Tuple2[IndividualInterface, IndividualInterface] = {

		var son1 = this.copy();
		son1.setFitness(-1.0);
		var son2 = partner.copy();
		son2.setFitness(-1.0);
		var positions = son1.differ(son2).toBuffer;

		var exchanges = positions.size / 2;
		if ((positions.size >0) && (exchanges == 0)) {
			exchanges = 1;
		}

		val rand = new Random();

		for (j <- 0 until exchanges) {
			var index = rand.nextInt(positions.size);
			var pos = positions(index);

			var aux = son1.chromosomes(pos);
			son1.chromosomes(pos) = son2.chromosomes(pos);
			son2.chromosomes(pos) = aux;

			positions.remove(index);
		}

		return Tuple2(son1, son2)
	}

	def updateFitness(trainSet: Array[TrainPoint], kBase :KnowledgeBase)

	def updateKBase(kBase :KnowledgeBase)

	def copy(): IndividualInterface

	def createHash(): String

}