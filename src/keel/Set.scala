package keel

import collection.mutable.ArrayBuffer

class Set{
	var name:String = "";
	var attributes: Array[Attribute] = Array[Attribute]()
	var input: Array[String] = Array[String]()
	var output: String = ""
	var data: Array[Array[String]] = Array[Array[String]]()

	def this(name: String){
		this();
		this.name = name	
	}

	def addAttribute(attribute: Attribute) = {
		this.attributes = this.attributes :+ attribute;
	}

	def setInput(input: Array[String]) = {
		this.input = input;
	}

	def setOutput(output: String) = {
		this.output = output;
	}

	def addPoint(point: Array[String]) = {
		this.data = this.data :+ point;
	}

	def getPoints(): Array[Tuple2[Array[String], String]] = {
		var inputsPos = ArrayBuffer[Int]();
		var result = ArrayBuffer[Tuple2[Array[String], String]]();
		var attributeNames = this.attributes.map(x => x.name);

		for( input <- this.input) {
			var index = attributeNames.indexOf(input);
			if(index != -1)
				inputsPos += index;
			else
				throw new Exception("Unknown input")
		}

		var outputPos = attributeNames.indexOf(this.output);
		if(outputPos == -1)
			throw new Exception("Unknown output")

		for(point <- this.data) {
			var antecedent = ArrayBuffer[String]()
			for( pos <- inputsPos) {
				antecedent += point(pos)
			}
			result += Tuple2(antecedent.toArray, point(outputPos))
		}

		result.toArray
	}

	def getParsePoints(): Array[Tuple2[Array[Double], Int]] = {
		var inputsPos = ArrayBuffer[Int]();
		var result = ArrayBuffer[Tuple2[Array[Double], Int]]();
		var attributeNames = this.attributes.map(x => x.name);

		for( input <- this.input) {
			var index = attributeNames.indexOf(input);
			if(index != -1)
				inputsPos += index;
			else
				throw new Exception("Unknown input")
		}

		var outputPos = attributeNames.indexOf(this.output);
		if(outputPos == -1)
			throw new Exception("Unknown output")

		for(point <- this.data) {
			var antecedent = ArrayBuffer[Double]()
			for( pos <- inputsPos) {

				antecedent += this.attributes(pos).getValue(point(pos))
			}
			result += Tuple2(antecedent.toArray, this.attributes(outputPos).getValue(point(outputPos)).toInt)
		}

		result.toArray
	}

	def getInputsAttributes(): Array[Attribute] = {
		var inputsPos = ArrayBuffer[Int]();
		var attributeNames = this.attributes.map(x => x.name);
		var result = ArrayBuffer[Attribute]();

		for( input <- this.input) {
			var index = attributeNames.indexOf(input);
			if(index != -1)
				inputsPos += index;
			else
				throw new Exception("Unknown input")
		}

		for( pos <- inputsPos) {
			result += this.attributes(pos)
		}

		result.toArray

	}

	def getOutputDomain(): Array[String] = {
		var attributeNames = this.attributes.map(x => x.name);
		var outputPos = attributeNames.indexOf(this.output);

		if(outputPos == -1)
			throw new Exception("Unknown output")

		this.attributes(outputPos).domain
	}

}