package keel

class Attribute{
	var name: String = ""
	var _type: String = ""
	var domain: Array[String] = Array[String]()
	def this(name: String, _type: String, domain:Array[String]){
		this();
		if(!Array("real", "integer", "nominal").contains(_type.toLowerCase()))
			throw new Exception("Unknown type")

		this.name = name
		this._type = _type.toLowerCase()

		if(Array("real", "integer").contains(this._type) && domain.length != 2)
			throw new Exception("Incorrect domain")

		this.domain = domain

	}

	def getValue(rawData: String): Double = {
		var result = 0.0
		if(this._type == "nominal")
			result = this.domain.indexOf(rawData)*1.0
		else
			result = rawData.toDouble

		result
	}

	def getRange(): Tuple2[Double, Double] = {
		var sup = 0.0
		var inf = 0.0
		if(this._type == "nominal")
			sup = this.domain.length*1.0
		else{
			inf = this.domain(0).toDouble
			sup = this.domain(1).toDouble
		}

		Tuple2(inf, sup)
	}

}