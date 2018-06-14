package fuzzyrules.utils

import collection.mutable.ArrayBuffer

class FuzzyRule{

    var antecedent: Array[Int] = Array();
    var consecuence: Int = -1;
    var ruleWeight: Double = -1;

    def this(antecedent: Array[Int], consecuence: Int, ruleWeight: Double) = {
        this()
        this.antecedent = antecedent;
        this.consecuence = consecuence;
        this.ruleWeight = ruleWeight;
    }

    def this(serializedRule: String, attributes: Array[Attribute], classNames: Array[String]) = {
        this()
        this.unserialize(serializedRule, attributes, classNames)
    }

    def equals(that: FuzzyRule): Boolean = {
        this.antecedent.sameElements(that.antecedent) && this.consecuence == that.consecuence;
    }

    def getMatchingDegree(point: Array[Double], attributes: Array[Attribute]): Double = {
        var matchingDegree = 1.0;
        for((value, i) <- point.zipWithIndex){
            matchingDegree = matchingDegree * attributes(i).getValueFunction(value, this.antecedent(i));
        }

        matchingDegree = matchingDegree * this.ruleWeight;

        matchingDegree;
    }

    def serialize(attributes: Array[Attribute], classNames: Array[String]): String = {
        var antecedentNames = ArrayBuffer[String]();
        for((antecedent, i) <- this.antecedent.zipWithIndex){
            antecedentNames += attributes(i).getName(antecedent);
        }

        var serializedRule = new StringBuilder(antecedentNames.toArray.mkString(","));
        serializedRule.append(";");
        serializedRule.append(classNames(this.consecuence));
        serializedRule.append(";");
        serializedRule.append(ruleWeight);

        serializedRule.toString();
    }

    def unserialize(serializedRule: String, attributes: Array[Attribute], classNames: Array[String]) = {
        val ruleParts = serializedRule.split(";");
        var antecedent = ruleParts(0).split(",");
        var antecedentBuffer = ArrayBuffer[Int]();
        for((attr, i) <- antecedent.zipWithIndex){
            antecedentBuffer += attributes(i).getIndex(attr);
        }
        this.antecedent = antecedentBuffer.toArray;
        this.consecuence = classNames.indexOf(ruleParts(1));
        this.ruleWeight = ruleParts(2).toDouble;
    }

    override def clone(): FuzzyRule = {
        new FuzzyRule(this.antecedent.clone(), this.consecuence, this.ruleWeight)
    }
}