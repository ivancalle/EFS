# Evolutionary Fuzzy System
This software implements Evolutionary Fuzzy System in Scala.
## Requeriments
- Scala 2.12
- Java Virtual Machine 8 (JVM8)
## Quickstarter
Clone this repository and create a file with name `main.scala` as this file
```scala
import scala.util.Random
import scala.io.Source
import collection.mutable.ArrayBuffer
import fuzzyrules.utils._
import fuzzyrules.classifiers._
import keel.Reader

object MainProject {
    def readTest(filename: String, classNames: Array[String]) = {
        val reader = new Reader(filename);
        reader.sets(0).getParsePoints().map(x => new TrainPoint(x._1, x._2));
    }

  def main(args: Array[String]): Unit = {
    if(args.length == 2){   
        println("Chi Algorith:")
        var classifier = new ChiClassifier(args(0));
        println(classifier.generateAndSummary(this.readTest(args(1), classifier.getKBase().getClassNames()), "Penalized_Certainty_Factor"));

        println("\n\nRule Selection Algorith:")
        var classifier2 = new ChiRSClassifier(args(0));
        println(classifier2.generateAndSummary(this.readTest(args(1), classifier2.getKBase().getClassNames()), "Penalized_Certainty_Factor"));

        println("\n\n2Tuple Algorith:")
        var classifier3 = new Chi2TupleClassifier(args(0));
        println(classifier3.generateAndSummary(this.readTest(args(1), classifier3.getKBase().getClassNames()), "Penalized_Certainty_Factor"));
    }else{
        println("incorrect number of parameters.")
    }
  }
}
```

After this, execute 
```sh
$ scala -cp project_path/evolutionary-fuzzy-rules.jar ./main.scala train_set_path.dat test_set_path.dat
```

This execute 3 algoritm over dataset and show the summary of times and accuracy in CSV format.
