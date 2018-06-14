package fuzzyrules.utils

import collection.mutable.ArrayBuffer

class TrainPoint{

    var values: Array[Double] = Array();
    var consecuence: Int = -1;

    def this(values: Array[Double], consecuence: Int) = {
        this()
        this.values = values;
        this.consecuence = consecuence;
    }

    def this(serializedPoint: String, classNames: Array[String]) = {
        this()
        this.unserialize(serializedPoint, classNames)
    }

    def serialize(classNames: Array[String]): String = {
        var valueString = this.values.map(x => x.toString)

        var serializedPoint = new StringBuilder(valueString.mkString(","));
        serializedPoint.append(",");
        serializedPoint.append(classNames(this.consecuence));

        serializedPoint.toString();
    }

    def unserialize(serializedRule: String, classNames: Array[String]) = {
        val ruleSplited = serializedRule.split(",");
        this.values = ruleSplited.init.map(x => x.toDouble);

        this.consecuence = classNames.indexOf(ruleSplited.last.trim);
    }

    def getValues(): Array[Double] = { return this.values }
    def getConsecuence(): Int = { return this.consecuence }

    def getMatchingDegree(antecedent: Array[Int], attributes: Array[Attribute]): Double = {
        var matchingDegree = 1.0;
        for((value, i) <- this.values.zipWithIndex){
            matchingDegree = matchingDegree * attributes(i).getValueFunction(value, antecedent(i))
        }

        matchingDegree;
    }

    def getAntecedent(attributes: Array[Attribute]): Array[Int] = {

        var antecedent = ArrayBuffer[Int]();

        for((value, i) <- this.values.zipWithIndex){
            antecedent += attributes(i).getValue(value);
        }

        antecedent.toArray;
    }
}