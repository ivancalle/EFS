package keel

import collection.mutable.ArrayBuffer
import scala.io.Source

class Reader{
	var sets: Array[Set] = Array[Set]()

	def this(filename: String){
		this();
		val bufferedSource = Source.fromFile(filename);

		val regexAttr = """^@attribute '?([\w_-]+)'? ([a-z]*) ?\[(-?[0-9.]+), ?(-?[0-9.]+)]$""".r;
		val regexAttrNominal = """^@attribute '?([\w_-]+)'? ?\{([^\}]+)\}$""".r;
		val regexInput = """^@inputs (.+)$""".r;
		val regexOutput = """^@outputs '?([\w_-]+)'?$""".r;
		val regexRelation = """^@relation +'?([\w_-]+)'?$""".r;
		var indexSet = -1;
		var sets = ArrayBuffer[Set]()

		for (line <- bufferedSource.getLines) {
			if(!line.startsWith("@")){
				sets(indexSet).addPoint(line.split(",").map( x => x.trim))

			}else {
				line match {
				  case regexAttr(name, _type, inf, sup) => {
				  	sets(indexSet).addAttribute(new Attribute(name, _type, Array(inf, sup)));
				  }
				  case regexAttrNominal(name, line) => {
				  	sets(indexSet).addAttribute(new Attribute(name, "nominal", line.split(",").map(x => x.trim)));
				  }
				  case regexInput(list) => {
				  	sets(indexSet).setInput(list.split(",").map(x => x.stripPrefix("'").stripSuffix("'").trim))

				  }
				  case regexOutput(clas) => {
				  	sets(indexSet).setOutput(clas)
				  }
				  case regexRelation(name) => {
				  	sets += new Set(name)
				  	indexSet = indexSet + 1
				  }
				  case _ =>
				}
			}
		}

		this.sets = sets.toArray;

		bufferedSource.close

	}
}