package fuzzyrules.classifiers

import fuzzyrules.utils._
import keel.Reader
import scala.io.Source
import collection.mutable.ArrayBuffer


abstract class GeneralClassifier{
	protected var trainSet: Array[TrainPoint] = null;
	protected var kBase: KnowledgeBase = null;

	def setAttrs(trainSet: Array[TrainPoint], kBase: KnowledgeBase) = {
		this.trainSet = trainSet;
		this.kBase = kBase;
	}

	def setAttrs(trainFilename: String, kBase: KnowledgeBase) = {
		this.kBase = kBase;
		this.readTrain(trainFilename);
	}
	
	def setAttrs(trainFilename: String) = {
		this.readTrain(trainFilename, false);
	}

	def readTrain(filename: String, onlyPoints: Boolean = true, intervalNum: Int = 3) = {  // intervalNum only works if onlyPoints = false
		if(!onlyPoints && intervalNum < 2)
			throw new Exception("Invalid intervalNum")

		val reader = new Reader(filename);
		val set = reader.sets(0);
		var trainSet = ArrayBuffer[TrainPoint]();

		for ((antecedent, clas) <- set.getParsePoints()) {
			trainSet += new TrainPoint(antecedent, clas);
		}

		if(!onlyPoints){
			var classNames = set.getOutputDomain();
			var attributes = ArrayBuffer[Attribute]();
			for( attr <- set.getInputsAttributes()) {
				if(attr._type == "nominal"){
					attributes += new Attribute(attr.name, attr.domain);
				}else{
					attributes += new Attribute(attr.name, attr.getRange(), intervalNum);
				}
				
			}

			this.kBase = new KnowledgeBase(Array[FuzzyRule](), attributes.toArray, classNames)
		}

		this.trainSet = trainSet.toArray
	}

	def generate(ruleWeightMode: String = "Certainty_Factor")

	def getTrainSet(): Array[TrainPoint] = {
		this.trainSet;
	}

	def getKBase(): KnowledgeBase = {
		this.kBase;
	}

	def classify(point: Array[Double]): Int = {
		this.kBase.classify(point);
	}

	def summary(examples: Array[TrainPoint]) : String = {
		var total = 0;
		var success = 0;
		val before = System.nanoTime();
		for(example <- examples) {
			var classIndex = this.classify(example.values);
			if(classIndex == example.consecuence){
				success = success + 1;
			}
			total = total + 1;
		}
		val after = System.nanoTime();
		val time = (after - before)/1000000000.0

		val accuracy = (1.0*success)/total;

		var summaryInfo = new StringBuilder("Info: \n\tClassify correctly: ");
		summaryInfo.append(success);
		summaryInfo.append("\n\tClassify wrongly: ");
		summaryInfo.append((total - success));
		summaryInfo.append("\n\tTotal: ");
		summaryInfo.append(total);
		summaryInfo.append("\n\tAccuracy: ");
		summaryInfo.append(accuracy);
		summaryInfo.append("\n\tTime: ");
		summaryInfo.append(time);
		summaryInfo.append("s\n");
		summaryInfo.append(this.kBase.getRulesInfo());

		summaryInfo.toString();
	}

	def generateAndSummary(examples: Array[TrainPoint], ruleWeightMode: String = "Certainty_Factor") : String = {
		val beforeGenerate = System.nanoTime();
		this.generate(ruleWeightMode);
		val afterGenerate = System.nanoTime();

		var total = 0;
		var success = 0;
		val beforeClassify = System.nanoTime();
		for(example <- examples) {
			var classIndex = this.classify(example.values);
			if(classIndex == example.consecuence){
				success = success + 1;
			}
			total = total + 1;
		}
		val afterClassify = System.nanoTime();
		val timeClassify = (afterClassify - beforeClassify)/1000000000.0;
		val timeGenerate = (afterGenerate - beforeGenerate)/1000000000.0;

		val accuracy = (1.0*success)/total;

		var summaryInfo = new StringBuilder("Info: \n\tClassify correctly: ");
		summaryInfo.append(success);
		summaryInfo.append("\n\tClassify wrongly: ");
		summaryInfo.append((total - success));
		summaryInfo.append("\n\tTotal: ");
		summaryInfo.append(total);
		summaryInfo.append("\n\tAccuracy: ");
		summaryInfo.append(accuracy);
		summaryInfo.append("\n\tTime of generation: ");
		summaryInfo.append(timeGenerate);
		summaryInfo.append("s\n\tTime of classification: ");
		summaryInfo.append(timeClassify);
		summaryInfo.append("s\n");
		
		summaryInfo.append(this.kBase.getRulesInfo());

		summaryInfo.toString();
	}

	def generateAndCSV(examples: Array[TrainPoint], ruleWeightMode: String = "Certainty_Factor") : String = {
		val beforeGenerate = System.nanoTime();
		this.generate(ruleWeightMode);
		val afterGenerate = System.nanoTime();

		var total = 0;
		var success = 0;
		val beforeClassify = System.nanoTime();
		for(example <- examples) {
			var classIndex = this.classify(example.values);
			if(classIndex == example.consecuence){
				success = success + 1;
			}
			total = total + 1;
		}
		val afterClassify = System.nanoTime();
		val timeClassify = (afterClassify - beforeClassify)/1000000000.0;
		val timeGenerate = (afterGenerate - beforeGenerate)/1000000000.0;

		val accuracy = (1.0*success)/total;
		success+";"+(total - success)+";"+total+";"+accuracy+";"+timeGenerate+"s;"+timeClassify+"s;"+(timeClassify+timeGenerate)+"s;"+this.kBase.size()
	}

}