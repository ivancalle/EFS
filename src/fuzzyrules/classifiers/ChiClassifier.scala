package fuzzyrules.classifiers

import fuzzyrules.utils._
import scala.collection.immutable.Map
import collection.mutable.ArrayBuffer


class ChiClassifier extends GeneralClassifier{
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
				if(!rulesTmp(keyAntecedent)._1.contains(point.consecuence)){
					rulesTmp(keyAntecedent)._1.append(point.consecuence);
				}
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