package fuzzyrules.genetics

import collection.mutable.ArrayBuffer
import scala.util.Random
import fuzzyrules.utils.KnowledgeBase
import fuzzyrules.utils.TrainPoint
import fuzzyrules.utils.Attribute

class Individual2Tuple extends IndividualInterface{
	def this(chromosomes: Array[Double]) = {
		this();
		var temp = ArrayBuffer[Any]()
		for( chromosome <- chromosomes) {
			temp += chromosome
		}
		this.chromosomes = temp.toArray;
	}

	def this(chromosomes: Array[Any], fitness: Double) = {
		this();
		this.chromosomes = chromosomes;
		this.fitness = fitness
	}

	def this(size: Int) = {
		this();
		val rand = new Random()
		var chromosomes = ArrayBuffer[Any]();
		for( i <- 0 until size) {
			chromosomes += rand.nextDouble()-0.5
		}
		this.chromosomes = chromosomes.toArray;
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

		var attributesArray = this.modifyAttr(kBase.getAttributes())
		for((rule, i)  <- kBase.getRules().zipWithIndex) {
			deggre = rule.getMatchingDegree(point, attributesArray);
			if(deggre > maxDegree){
				maxDegree = deggre;
				classIndex = rule.consecuence;
			}
		}

		classIndex;
	}

	def modifyAttr(originalAttr: Array[Attribute]): Array[Attribute]= {
		var attributes = ArrayBuffer[Attribute]()
		for( attr <- originalAttr) {
			attributes += attr.clone()
		}
		var attributesArray = attributes.toArray
		var attrIndex: Int = 0
		var funcIndex: Int = 0

		var attr: Attribute = attributesArray(attrIndex)
		for((chromo,i) <- this.chromosomes.zipWithIndex) {
			var chromoDouble = chromo.asInstanceOf[Double]
			attr = attributesArray(attrIndex)

			if(funcIndex == 0){
				attr.move(0,attr.getGap(0,1)*chromoDouble)
			}else if(funcIndex == (attr.size()-1))
				attr.move(funcIndex,attr.getGap((funcIndex-1),funcIndex)*chromoDouble)
			else if(chromoDouble < 0.0)
				attr.move(funcIndex,attr.getGap((funcIndex-1),funcIndex)*chromoDouble)
			else
				attr.move(funcIndex,attr.getGap(funcIndex,(funcIndex+1))*chromoDouble)

			funcIndex = funcIndex + 1
			if(funcIndex == attr.size()){
				funcIndex = 0
				attrIndex = attrIndex + 1
			}
		}

		attributesArray
	}

	def updateKBase(kBase :KnowledgeBase)  = {
		kBase.setAttributes(this.modifyAttr(kBase.getAttributes()))
	}

	def createHash(): String = {
		this.chromosomes.mkString(",")
	}

	override def copy(): IndividualInterface = {
		new Individual2Tuple(this.chromosomes.clone(), this.fitness)
	}
}