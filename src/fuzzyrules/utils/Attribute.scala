package fuzzyrules.utils

import collection.mutable.ArrayBuffer

class Attribute{
	var name:String = null;
	private var functions: Array[MembershipFunction] = null;

	def this(name: String, functions: Array[MembershipFunction]) = {

		this();
		this.name = name;
		this.functions = functions;
		for(func <- functions){
			func.normalize();
		}
	}

	def this(name: String, domain: Tuple2[Double, Double], numIntervals: Int) = {

		this();
		this.name = name;
		var functions = ArrayBuffer[MembershipFunction]()
		val intervalLength = (domain._2-domain._1)/(numIntervals-1)
		for( i <- 0 to (numIntervals-1)) {
			if(i == 0){
				functions.append(new MembershipFunction("L_"+i, Array(Tuple2(domain._1, 1.0), Tuple2(domain._1+intervalLength, 0.0))))
			}else if(i == (numIntervals-1)){
				functions.append(new MembershipFunction("L_"+i, Array(Tuple2(domain._2-intervalLength, 0.0), Tuple2(domain._2, 1.0))))
			}else{
				val first = domain._1+intervalLength*(i-1)
				val second = domain._1+intervalLength*(i)
				val third = domain._1+intervalLength*(i+1)
				functions.append(new MembershipFunction("L_"+i, Array(Tuple2(first, 0.0), Tuple2(second, 1.0), Tuple2(third, 0.0))))
			}
		}
		this.functions = functions.toArray;
	}

	def this(name: String, listNames: Array[String]) = {

		this();
		this.name = name;
		var functions = ArrayBuffer[MembershipFunction]()
		for( (name, i) <- listNames.zipWithIndex) {
			functions.append(new MembershipFunction(name, Array(Tuple2(i-1, 0.0), Tuple2(i, 1.0), Tuple2(i+1, 0.0))))
		}
		this.functions = functions.toArray;
	}

	def serialize(): String = {
		var serializedAttr = new StringBuilder(this.name);
		serializedAttr.append("\n");
		for(func <- this.functions){
			serializedAttr.append("\t");
			serializedAttr.append(func.serialize());
			serializedAttr.append("\n");
		}
		serializedAttr.toString()
	}


    /**
	 * Get index of function of maximize the value
	 * @param value of attribute 
	 * @return index of the function
	 */
	def getValue(x: Double): Int = {
		var max = -1.0;
		var index = -1;
		
		for((function, i) <- this.functions.zipWithIndex){
			var value = function.eval(x);
			if(value >= max){
				max = value;
				index = i;
			}
		}

		index;
	}

	/**
	 * Get index of function of maximize the value
	 * @param value of attribute 
	 * @return index of the function
	 */
	def getValueFunction(x: Double, index: Int): Double = {
		this.functions(index).eval(x);
	}

	def getIndex(name: String): Int = {
		var index: Int = -1;
		for((function, i) <- this.functions.zipWithIndex.iterator.takeWhile(_ => index == -1)){
			if(name == function.name){
				index = i;
			}
		}
		index
	}

	def getTags(): Array[String] = {
		this.functions.map(x => x.name)
		
	}

	def move(index: Int, amount: Double) = {
		this.functions(index).move(amount)
		
	}

	/**
	 * Get name name of function
	 * @param index of funtion
	 * @return name of function
	 */
	def getName(index: Int): String = {
		if(0 <= index && index < this.functions.length){
			this.functions(index).name;
		}else{
			"No name";
		}
	}

	def getGap(first: Int, second: Int): Double = {
		var res = 0.0 
		if(first < second){
			res = this.functions(second).firstValue() - this.functions(first).lastValue()
		}else if(first < second){
			res = this.functions(first).firstValue() - this.functions(second).lastValue()
		}else{
			res = this.functions(first).lastValue() - this.functions(second).firstValue()
		}

		res
	}

	def size(): Int = {
		this.functions.length
	}

	override def clone(): Attribute = {
		var functions = ArrayBuffer[MembershipFunction]()
		for( func <- this.functions) {
			functions += func.clone()
		}
		new Attribute(this.name, functions.toArray);
	}


}