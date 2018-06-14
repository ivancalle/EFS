package fuzzyrules.utils

import collection.mutable.ArrayBuffer

class MembershipFunction{
    var name: String = null;
    private var points: Array[Tuple2[Double,Double]] = null;

    def this(name: String, points: Array[Tuple2[Double,Double]]) = {
        this();
        this.name = name;
        this.points = points.sortBy(x => x._1);
    }

    def this(serializedFunction: String) = {
        this();
        this.unserialize(serializedFunction);
    }

    /**
	 * Set a object with a string with this pattern "name;x1,y1;x2,y2;....;xn,yn"
	 * @param string with serialized membership function
	 */
    def unserialize(serializedFunction: String) = {
        val arrayFunction = serializedFunction.split(";")
        if (arrayFunction.length < 3){
        	print("Error");

        }else{
        	this.name = arrayFunction(0);
            var points = ArrayBuffer[Tuple2[Double,Double]](); 
        	for(point <- arrayFunction.tail){
        		val arrayPoint = point.split(",").map(x => x.toDouble);
        		if(arrayPoint.length >= 2){
        			points += Tuple2(arrayPoint(0), arrayPoint(1));
        		}
        		
        	}
        	this.points = points.toArray.sortBy(x => x._1)

        }
    }

    /**
     * Serialize Function in "name;x1,y1;x2,y2;....;xn,yn"
     * @return string with serialized membership function
     */
    def serialize(): String = {
        var serializedFunction = this.name + ";";
        for(point <- this.points){
            val pointStr = point._1 + "," + point._2 + ";";
            serializedFunction += pointStr;
        }

        serializedFunction.init
    }

    /**
     * Evalue Function in x
     * @return value of function in x
     */
    def eval(x: Double): Double = {
        var firstInterval = -1;
        var lastInterval = -1;

        for((point, i) <- this.points.zipWithIndex.iterator.takeWhile(_ => lastInterval == -1)){
            if(point._1 > x){
                lastInterval = i;
            }else{
                firstInterval = i;
            }
        }

        if(firstInterval == -1){
            this.points(0)._2;
        }else if(lastInterval == -1){
            this.points.last._2;
        }else if(firstInterval != -1 && lastInterval != -1){
            val x1 = this.points(firstInterval);
            val x2 = this.points(lastInterval);
            (x2._2-x1._2)/(x2._1-x1._1)*(x-x1._1) + x1._2;
        }else{
            throw new Exception("Lista de puntos vacía");
        }
    }

    def normalize() = {
        val sortedArray = this.points.sortBy(x => x._2)
        val max = sortedArray.last._2;
        val min = sortedArray(0)._2;

        for(i <- 1 to (this.points.length-1)){
            this.points(i) = Tuple2(this.points(i)._1,(this.points(i)._2 - min)/max);
        }
    }

    def lastValue(): Double = {
        var last: Double = 0.0
        for((point, i) <- this.points.zipWithIndex){
            if(point._2 == 1.0){
                if(i < (this.points.length-1))
                    last = point._1
                else
                    last = 0.0
            }
        }
        last
    }

    def move(amount:Double) = {
        for((point, i) <- this.points.zipWithIndex){
            this.points(i) = Tuple2(point._1 + amount, point._2)
        }
    }

    def firstValue(): Double = {
        var first: Double = 0.0
        var find = false
        for((point, i) <- this.points.zipWithIndex.iterator.takeWhile(_ => !find)){
            if(point._2 == 1.0){
                if(i > 0)
                    first = point._1

                find = true
            }
        }
        first
    }

    override def clone(): MembershipFunction = {
        new MembershipFunction(this.name, this.points.clone())
    }
}