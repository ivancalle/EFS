package fuzzyrules.utils

import scala.io.Source
import java.io.PrintWriter
import collection.mutable.ArrayBuffer


class KnowledgeBase{
	private var rules: Array[FuzzyRule] = Array();
	private var attributes: Array[Attribute] = Array();
	private var classNames: Array[String] = Array();

	def this(rules: Array[FuzzyRule], attributes: Array[Attribute], classNames: Array[String]) = {
        this();
        this.rules = rules;
        this.attributes = attributes;
        this.classNames = classNames;
    }

    def this(filename: String) = {
        this();
        this.readModel(filename);
    }

	def writeModel(filename: String) = {
		var fileContent = new StringBuilder(this.classNames.mkString(";"));
		fileContent.append("\n");

		for(attr <- this.attributes){
			fileContent.append(attr.serialize());
		}

		fileContent.append("------------------------------\n");
		
		for(rule <- this.rules){
			fileContent.append(rule.serialize(this.attributes, this.classNames));
			fileContent.append("\n");
		}

		new PrintWriter(filename) { write(fileContent.toString()); close }
	}

	def readModel(filename: String) = {
		val bufferedSource = Source.fromFile(filename);
		var clases = bufferedSource.getLines.next();
		this.classNames = clases.split(";");

		var name = "";
		var functions = ArrayBuffer[MembershipFunction]();
		var attributes = ArrayBuffer[Attribute]();
		var rules = ArrayBuffer[FuzzyRule]();
		var endAttr = false;

		for (line <- bufferedSource.getLines) {
			if(!endAttr){
			    if(line.startsWith("\t")){
			    	functions += new MembershipFunction(line.tail);
		    	}else if(line == "------------------------------"){
		    		endAttr = true;
		    		attributes += new Attribute(name,functions.toArray);
		    		this.attributes = attributes.toArray;
		    	}else{
		    		if(name != ""){
		    			attributes += new Attribute(name,functions.toArray);
		    		}
		    		name = line;
		    		functions = ArrayBuffer[MembershipFunction]();
		    	}
		    }else{
				var rule = new FuzzyRule(line, this.attributes, this.classNames);
			    rules += rule;

		    }
		}
		
		this.rules = rules.toArray;

		bufferedSource.close
	}

	def addRule(rule: FuzzyRule) = {
		this.rules = this.rules :+ rule;
	}

	def updateSelection(selection: Array[Boolean]) = {
		var rulesBuff = this.rules.toBuffer;
		for( i <- (selection.size-1) to 0 by -1) {
			if(!selection(i)){
				rulesBuff.remove(i);
			}
		}
		this.rules = rulesBuff.toArray;
	}

	def ClassMostFrequent(): Int = {
		var counts = Array.fill[Int](this.classNames.length)(0);
		for(rule  <- this.rules) {
			counts(rule.consecuence) = counts(rule.consecuence) + 1;
		}

		var classIndex = -1;
		var maxCount = 0;
		for((count, i) <- counts.zipWithIndex) {
			if(count > maxCount){
				classIndex = i;
				maxCount = count;
			}
		}

		classIndex;
	}

	def classify(point: Array[Double]): Int = {
		var maxDegree = 0.0;
		var classIndex = this.ClassMostFrequent();  // cambiar por ? y poner en parametro

		for(rule  <- this.rules) {
			var deggre = rule.getMatchingDegree(point, this.attributes);
			if(deggre > maxDegree){
				maxDegree = deggre;
				classIndex = rule.consecuence;
			}
		}

		classIndex;
	}

	def getAccuracy(examples: Array[TrainPoint]): Double = {
		var success = 0;
		var total = 0;
		for(example <- examples) {
			var classIndex = this.classify(example.values);
			if(classIndex == example.consecuence){
				success = success + 1;
			}
			total = total + 1;
		}

		(1.0*success)/total;
	}

	def size(): Int = {
		this.rules.length;
	}

	def getRule(index: Int): FuzzyRule = {
		this.rules(index);
	}

	def getRules(): Array[FuzzyRule] = {
		this.rules;
	}

	def removeRuleList(list: Array[Int]) = {
		var orderList = list.sortWith(_ > _);
		if(orderList.length > 0 && orderList(0) < this.rules.length){
			var ruleBuffer = this.rules.toBuffer
			for( i <- orderList) {
				ruleBuffer.remove(i)
			}
			this.rules = ruleBuffer.toArray
		}
	}

	def contains(rule: FuzzyRule): Boolean = {
		var find = false;
		for(auxRule <- this.rules.iterator.takeWhile(_ => !find)){
			if(rule.equals(auxRule)){
				find = true;
			}
		}
		find
	}

	def cleanRules() = {
		this.rules = this.rules.filter(_.ruleWeight > 0)
	}

	def getAttributes(): Array[Attribute] = {
		this.attributes;
	}

	def setAttributes(attributes: Array[Attribute]) = {
		this.attributes = attributes;
	}

	def getClassNames(): Array[String] = {
		this.classNames;
	}

	def getRulesInfo(): String = {
		var counts = Array.fill[Int](this.classNames.length)(0);
		for(rule  <- this.rules) {
			counts(rule.consecuence) = counts(rule.consecuence) + 1;
		}

		var info = new StringBuilder("Info Rules: \n\tFinal number of rules: ");
		info.append(this.rules.length);
		info.append("\n\tRules per class:");
		for((count, i) <- counts.zipWithIndex) {
			info.append("\n\t\t");
			info.append(this.classNames(i));
			info.append(": ");
			info.append(count);
		}

		info.toString();
	}

	override def clone(): KnowledgeBase = {
		var rules = ArrayBuffer[FuzzyRule]()
		for( rule <- this.rules) {
			rules += rule.clone()
		}

		var attributes = ArrayBuffer[Attribute]()
		for( attribute <- this.attributes) {
			attributes += attribute.clone()
		}

		new KnowledgeBase(rules.toArray, attributes.toArray, this.classNames.clone());
	}

}