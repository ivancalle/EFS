package fuzzyrules.classifiers

import fuzzyrules.utils._
import scala.collection.immutable.Map
import collection.mutable.ArrayBuffer
import fuzzyrules.genetics._
import scala.util.Random


class Chi2TupleClassifier extends GeneralClassifier{
	def this(trainSet: Array[TrainPoint], kBase: KnowledgeBase) = {this(); super.setAttrs(trainSet, kBase);}
	def this(trainFilename: String, kBase: KnowledgeBase) = {this(); super.setAttrs(trainFilename, kBase);}
	def this(trainFilename: String) = {this(); super.setAttrs(trainFilename);}

	def generate(ruleWeightMode: String = "Certainty_Factor") = {
		if(!Array("Certainty_Factor", "Penalized_Certainty_Factor").contains(ruleWeightMode))
			throw new Exception("Unknown ruleWeightMode")
			
		var rulesTmp = Map[String, Tuple2[ArrayBuffer[Int], Map[Int,Double]]]();
		var attrs = this.kBase.getAttributes();
		for(point <- this.trainSet){
			var antecedent = point.getAntecedent(attrs);
			var keyAntecedent = antecedent.mkString(";");
			if((rulesTmp get keyAntecedent) == None){
				rulesTmp += (keyAntecedent -> Tuple2(ArrayBuffer[Int](point.consecuence), this.getValues(antecedent)));
			}else{
				rulesTmp(keyAntecedent)._1.append(point.consecuence);
			}
		}

		if(ruleWeightMode == "Certainty_Factor"){
			for((keyAnte, tuple) <- rulesTmp){
				var classIndex = -1;
				var ruleWeight = 0.0;

				val consecuences = tuple._2

				for((index, weight) <- consecuences){
					if(tuple._1.contains(index)){
						if(weight > ruleWeight){
							classIndex = index;
							ruleWeight = weight;
						}
					}
				}

				this.kBase.addRule(new FuzzyRule(keyAnte.split(";").map(x => x.toInt), classIndex, ruleWeight))
			}
		}else if(ruleWeightMode == "Penalized_Certainty_Factor"){
			for((keyAnte, tuple) <- rulesTmp){
				var classIndex = -1;
				var ruleWeight = -1.0;
				var totalSum = 0.0;

				val consecuences = tuple._2

				for((index, weight) <- consecuences){
					totalSum = totalSum + weight;
				}

				for((index, weight) <- consecuences){
					if(tuple._1.contains(index)){
						var weightSum = (2*weight - totalSum)/totalSum
						if(weightSum > ruleWeight){
							classIndex = index;
							ruleWeight = weightSum;
						}
					}
				}

				if(ruleWeight > 0.0){
					this.kBase.addRule(new FuzzyRule(keyAnte.split(";").map(x => x.toInt), classIndex, ruleWeight))
				}
			}

		}

		this.executeGenetic().updateKBase(this.kBase)

	}

	private def executeGenetic(): Individual2Tuple = {
		var pop = new Population(this.kBase, this.trainSet, 5000);
		var sum = 0;
		for( attr <- this.kBase.getAttributes()) {
			sum = sum + attr.size()
		}
		var chromos = Array.fill(sum)(0.0);
		var best = new Individual2Tuple(chromos);
		var individuals = generateRandomPopulation(this.kBase.size()-1);
		individuals += best;
		val rulesLength = this.kBase.size()

		pop.initialize(individuals.toArray);

		var resets = 0;
		var newIndividuals = pop.updateFitness();
		var bestFitness = -1.0
		do{	
			pop.cross();
			newIndividuals = pop.updateFitness();
			pop.select(rulesLength)
			best = pop.getFirsts(1)(0).asInstanceOf[Individual2Tuple];
			if (best.getFitness > bestFitness){
				bestFitness = best.getFitness;
				resets = 0;
			}
			if (!newIndividuals){
				pop.threshold -= 1;
				if (pop.threshold < 0){
					individuals = generateRandomPopulation(this.kBase.size()-1);
					individuals += best
					pop.initialize(individuals.toArray);
					newIndividuals = pop.updateFitness();
					pop.threshold = kBase.size()/4.0;
		            resets += 1;             
				}
			} 
		}while((pop.nEvals > 0)&&(best.getFitness < 1.0)&&(resets < 3));

		best
	}

	private def generateRandomPopulation(n: Int): ArrayBuffer[Individual2Tuple] = {
		var individuals = ArrayBuffer[Individual2Tuple]();
		var sum = 0;
		for( attr <- this.kBase.getAttributes()) {
			sum = sum + attr.size()
		}
		for( j <- 0 until n) {
			individuals += new Individual2Tuple(sum);
		}

		individuals
	}

	private def getValues(antecedent: Array[Int]): Map[Int,Double] = {
		var value = Map[Int,Double]();
		var attrs = this.kBase.getAttributes();
		for(point <- this.trainSet){
			if((value get point.consecuence) == None){
				value += (point.consecuence -> point.getMatchingDegree(antecedent, attrs));

			}else{
				var valueDouble: Double = point.getMatchingDegree(antecedent, attrs) + value(point.consecuence);
				value += (point.consecuence -> valueDouble);
			}
		}
		value
	}

}