import scala.io._
import java.io._
//import java.io.File

import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import org.jsoup.nodes.Element
import org.jsoup.select.Elements

import scala.util.matching.Regex
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.collection.parallel._
import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack
import java.io.PrintWriter
import libsvm._

object Main {

  // タグ・数値変換データ用HashMap
  val convertMap = scala.collection.mutable.HashMap.empty[String, Int]
  var convertFname = ""
  var cnvAddFlag = false

  def main(args: Array[String]): Unit = {

	/*
    var idx = 0
    try {
      while(idx < args.size) {
        args(idx) match {
          case """-cnv""" =>
            // タグ・数値変換データファイル
            convertFname = args(idx+1)
            idx += 2
          case """-add""" =>
            // タグ・数値変換データファイルへの追加登録
            cnvAddFlag = true
            idx += 1
        }
      }
    } catch {
      case e: Exception =>
        System.out.println("""Illeagal argument...""")
        sys.exit(1)
    }
	*/

    if(!convertFname.isEmpty()) {
      // タグ・数値変換データをHashMap に設定(UTF-8)
      val cnvData = Source.fromFile(convertFname, "UTF-8")
      cnvData.getLines().foreach{ line: String =>
        if(!line.isEmpty() && line.length > 3) {
          val list = line split ','
          convertMap += (list(0) -> list(1).toInt)
        }
      }
    }

  var problemFile = "./problem.txt"
  var trainFile = "./train.tree"
  //var testFile = "./test.tree"
  var interactive = false
  var verification = ""

  def readConfig(configFile: String) {
    val attributeP = new Regex("""\s*([^:\s]+)\s*:\s*(.*)""")
    val lines = Source.fromFile(configFile).getLines
    for(l <- lines) {
      l match {
        case attributeP(attr, value) =>
          attr.toLowerCase match {
            case "optimization" => problemFile = value
            case "train" => trainFile = value
            //case "test" => testFile = value
            case "interactive" => interactive = true
			case "verification" => verification = value
            case _ => // Do nothing
          }
        case _ => //Do nothing
      }
    }
  }

      // ファイルから一行ずつ木をとってきて、line_arrayに格納するための関数
    def readFile(filename: String): ArrayBuffer[String] = {
      val source = Source.fromFile(filename, "UTF-8")
      var line_array = new ArrayBuffer[String]

      try {
        source.getLines() foreach { line: String => line_array += line
        }
      } finally {
        source.close()
      }
     line_array
    }


	readConfig(args(0))

    /*
     Read a problem setting
     */

    var alpha = 0.0
    var beta = 0.0
    var logC = 0.0
    var norm = false
    var flagLabels = false
    var flagSamples = false
    var flagDiag = false
    var labels = Array[Double]()
    var diag = Array[Double]()
    val tempSamples = ArrayBuffer[Array[libsvm.svm_node]]()

    for(l <- Source.fromFile(problemFile).getLines) {
      val temp = l.split("""\s+""")
      if(temp(0) == "***") {
        temp(1) match {
          case "Optimal" =>
            temp(2) match {
              case "X" => alpha = temp(4).toDouble
              case "Y" => beta = temp(4).toDouble
              case "log" => logC = temp(5).toDouble
              case _ => //Do nothing.
            }
          case "Normalize" => norm = if(temp(3) == "t") true else false
          case "Labels:" => flagLabels = true
          case "Samples:" => flagSamples = true
          case "Diagonal" => flagDiag = true
          case _ => //Do nothing.
        }
      } else if(flagLabels == true) {
        labels = l.split(",").map(_.toDouble).toArray
        flagLabels = false
      } else if(flagDiag == true) {
        diag = l.split(",").map(_.toDouble).toArray
		//diag.foreach{ x => println(x)}
        flagDiag = false
      } else if(flagSamples == true) {
        tempSamples += l.split(" ").map{each =>
          val Array(index, value) = each.split(":")
          val node = new libsvm.svm_node
          node.index = index.toInt
          node.value = value.toDouble
          node
        }
      }
    }

    val samples = tempSamples.toArray

    println("*** Parameter setting")
    println("Alpha = " + alpha)
    println("Beta = " + beta)
    println("Log C = " + logC)
    if(norm) println("Kernel values are normalized.")

	//val ptk = new PTK(alpha)
    /*
     Read .tree files for training and test
     */

    val train_text = readFile(trainFile)

    val trainData = train_text.par.map{l =>
      val array = l.split(":")
      (array(0).toDouble, array(1))
    }.toArray

	val train_treeData = trainData.map{x =>
	   val invalidLabel = -1
       val dummyNode = new TreeNode(invalidLabel, ArrayBuffer[TreeNode]())
	   val tree = TreeParser(convertMap).parse(x._2).getOrElse(dummyNode)
	   tree
	}
    def getTrainTree(index: Int): TreeNode = train_treeData(index - 1)
    def getTrainLabel(index: Int): Double = labels(index - 1)
    def getDiagonalValue(index: Int): Double = diag(index - 1)
    def getURL(index: Int): String = "#"+index

    /*
     Verify consistency
     */

    println("*** Verifying consistency of the input")

    val trainDiag = train_treeData.par.map{tree =>
	  val ptk = new PTK(alpha)
      ptk.eval(tree, tree)
    }

    // DEBUG
    println(diag.zip(trainDiag).map(x => x._1 +":"+ x._2).mkString(" "))

    var errorFlag = false
    for(p <- diag.zip(trainDiag)) {
      if(p._1 != p._2) errorFlag = true
    }
    if(errorFlag) {
	  val diff: Array[Double] = diag.zip(trainDiag).map(x => (Math.abs(x._1 - x._2))/x._1)
	  println("Rounding Error: " + diff.max)
      println("Do you like to abort? (y/n)")
      var command = ""
      while(command != "n") {
        command = scala.io.StdIn.readLine
        if(command == "y") return
      }
    } else {
      println("Congratulations!")
      println("Verification successful!")
    }

    /*
     Train SVM
     */

    val prob = new libsvm.svm_problem()
    prob.x = samples
    prob.y = labels
    prob.l = labels.size

    val param = new libsvm.svm_parameter()
    param.svm_type = svm_parameter.C_SVC
    param.kernel_type = svm_parameter.PRECOMPUTED
    param.C = math.pow(10,logC)

    val model = svm.svm_train(prob, param)
    val support_vecs = model.SV.map(_(0).value)

    /*
     Define the prediction function
     */

    val empty_node = new libsvm.svm_node
    empty_node.index = 0
    val term_node = new libsvm.svm_node
    term_node.index = -1

	def predict(target: String, convertMap: HashMap[String, Int], mode: Int): Double = {
	  var targetStr = ""
	  val invalidLabel = -1
      val dummyNode = new TreeNode(invalidLabel, ArrayBuffer[TreeNode]())
	  var targetTree = new TreeNode(invalidLabel, ArrayBuffer[TreeNode]())
	  
	  if(mode == 1){
	    // ファイル渡ししてる
		var htmlStr = ""
			try { 
				val htmlList = Html.getSource(target)
				htmlStr = htmlList.mkString
			} catch {
				case e:Exception => println("URL_ERROR: " + e.getMessage)
			}
			val htmlFile = new File("htmlFile")
			val filewriter = new FileWriter(htmlFile)
			filewriter.write(htmlStr)
			filewriter.close()
			targetStr = sanitize(htmlFile).getOrElse("")
			//println("targetStr: " + targetStr)
		targetTree = IntTreeParser.parse(targetStr).getOrElse(dummyNode)
		println("targetTree: " + targetTree)
	  }
	  else if(mode == 2){
		targetStr = target
		targetTree = TreeParser(convertMap).parse(targetStr).getOrElse(dummyNode)
	  }

      val test_vec = empty_node +: (1 to samples.size).par.map{index =>
        val node = new libsvm.svm_node
        node.index = index
        if(support_vecs.contains(index)) {
          val ptk = new PTK(alpha)
		  val trainTree: TreeNode = getTrainTree(index)
          val temp = ptk.eval(targetTree, trainTree)
          if(norm)
            { node.value = temp/math.sqrt(ptk.eval(targetTree, targetTree) * diag(index - 1))
              println("node.value: " + node.value)
			  }
          else { node.value = temp
			  println("node.value: " + node.value)
			  }
        }else {
          node.value = 0.0
        }
        node
      }.toArray :+ term_node
      svm.svm_predict(model, test_vec)
    }

    if(interactive) {
      var continue = true

      while(continue) {
          println("Please input target(URL/Tree) or  Type 'q' to abort.")
    	  val target = scala.io.StdIn.readLine
      	  if(target == "q") continue = false
      	  else{
			  //try{ print(if(predict(target, convertMap, 1) > 0) "\n<FAKE>\n" else "\n<AUTHENTIC>\n") } catch { case e:Exception => print("\n<Error: Unpredictable>\n") }
			  print(if(predict(target, convertMap, 1) > 0) "\n<FAKE>\n" else "\n<AUTHENTIC>\n")
			  println
    	  }
      }
    } else if (verification.length != 0) {
		val acc: Double = verify(verification)
		println("accuracy: " + acc)
    }
    println
    println("Service terminated!  Thank you!")


  def sanitize(htmlFile: File): Option[String] =  {
      val sanitizer = new htmlSanitizer(convertMap, cnvAddFlag)
      var cntConvert = 0
      var convertHtml: Option[String] = None
  		// サニタイジング&木構造データ作成
      try {
        convertHtml = sanitizer.sanitizeJsoup(htmlFile)
      } catch {
        case e: Exception =>
        System.out.println("Jsoup_ERROR: " + e.getMessage)
      }
    //-- 木構造データ作成 End

    // タグ・数値変換データの出力
	/*
    val mapStr = sanitizer.getConvertMap()
    mapStr match {
      case Some(x) =>
        // タグ・数値変換データファイルを上書き(UTF-8)
        val convertFile = new File(convertFname)
        val pWriter = new PrintWriter(new BufferedWriter(new OutputStreamWriter(new FileOutputStream(convertFile),"UTF-8")))
        pWriter.write(x)
        pWriter.close()
        System.out.println("Convert file updated: " + convertFname)
      case None =>
        // do nothing
    }
	*/
    convertHtml
   }
   
   def verify (testfile: String): Double = {
      var line_array = readFile(testfile)
	  val result: ArrayBuffer[Int] = line_array.map { line =>
        val index = line.indexOf(":")
		val label = line.substring(0,index).toInt
		val target = line.substring(index+1)
		//val prediction = try{ if(predict(target, convertMap, 2) > 0) 1 else -1} catch { case e:Exception => print("\n<Error: Unpredictable>\n") }
		val prediction = if(predict(target, convertMap, 2) > 0) 1 else -1
		val true_cnt: Int = if (label == prediction) 1 else 0
		println("true_cnt: " + true_cnt)
		true_cnt
      }
	  ((result.sum).toDouble)/((result.length).toDouble)
    }
  }
}

object IntTreeParser {
  def parse(src:String): Option[TreeNode] = {
    val p = new Regex("""([^,\s\(\)]*)([,\s\(\)]?)\s*(.*)""")
    val i_root = TreeNode(-1, ArrayBuffer[TreeNode]())
    var crr_n = i_root
    var delim = "("
    var remain = src
    while (remain != ""){
      val p(label,d,r) = remain
      delim = d
      remain = r
      if(label == "") {
        if(delim == ")") {
          if(crr_n.up != None) crr_n = crr_n.parent.get else return None
        }
      } else {
        val new_n = TreeNode((label.toInt), ArrayBuffer[TreeNode]())
        new_n.setParent(crr_n)
        crr_n.children += new_n
        delim match {
          case ")" =>
            crr_n.parent match {
              case Some(p) => crr_n = p
              case None => return None
            }
          case "(" =>
            crr_n = new_n
          case "" =>
          // do nothing
          case _ =>
            // do nothing
        }
      }
    }
    if(crr_n == i_root) {
      val rtn_n = i_root.children(0)
      rtn_n.parent = None
      Some(rtn_n)
    } else {
      None
    }
  }
}

//頂点のラベルがStringの場合
// 引数のmapは、StringラベルをIntラベルに変換するためのHashMapの生成
case class TreeParser(convertMap: HashMap[String, Int]) {

  def parse(src:String): Option[TreeNode] = {
    val p = new Regex("""([^,\s\(\)]*)([,\s\(\)]?)\s*(.*)""")
    val i_root = TreeNode(0, ArrayBuffer[TreeNode]())
    var crr_n = i_root
    var delim = "("
    var remain = src
    var c = 0

    var num_label = convertMap.size+2
	//var num_label = 2
    var idx = 0

    while (remain != ""){
      var p(label,d,r) = remain
      delim = d
      remain = r

      if(label == "") {
        if(delim == ")") {
          if(crr_n.up != None) crr_n = crr_n.parent.get else return None
        }
      } else {
          if(convertMap.get(label) == None){
            idx = num_label + 1
            convertMap += (label -> idx)
            num_label += 1
          }else{
            idx = convertMap.get(label).get
          }

        val new_n = TreeNode(idx, ArrayBuffer[TreeNode]())
        new_n.setParent(crr_n)
        crr_n.children += new_n
        delim match {
          case ")" =>
            crr_n.parent match {
              case Some(p) => crr_n = p
              case None => return None
            }
          case "(" =>
            crr_n = new_n
          case "" =>
          // do nothing
          case _ =>
            // do nothing
        }
      }
    }
    if(crr_n == i_root) {
      val rtn_n = i_root.children(0)
      rtn_n.parent = None
      Some(rtn_n)
    } else {
      None
    }
  }
}


case class SubPath(entity: ArrayBuffer[Int]) {

  // SubPathを辞書式順にソートする際に用いる、大小関係の定義
  def <= (y: SubPath): Boolean = {
    if(entity.size == 0) return true
    if(y.entity.size == 0) return false
    if(entity(0) < y.entity(0)) return true
    if(entity(0) == y.entity(0)) {
      var i = 1

      // SubPathの一つ目のentityが同じだった場合の判定処理
      //(全てのentityが同じだった場合には、サイズの大きい方を。サイズまで同じ場合には、元の木のIndexの小さい(若い)方を、値が大きいものとして扱う)
      while( ((i <= entity.size-1) && (i <= y.entity.size-1)) && entity(i) == y.entity(i)){
        i += 1
      }
      if((i == entity.size) || (i == y.entity.size)){
        if (entity.size < y.entity.size)return true
        else return false
      }
      else{
        if(entity(i) < y.entity(i)) return true
        else return false
      }
    }
    if(entity(0) > y.entity(0)) return false
       return new SubPath(entity.tail) <= new SubPath(y.entity.tail)
  }

  def add (label: Int) = {
    entity += label
  }

  def size (): Int = {
    entity.size
  }

  def element (index: Int): Int = {
    entity(index)
  }

  def last (): Int = {
    entity(entity.length-1)
  }

}

case class TreeNode(label: Int, children: ArrayBuffer[TreeNode]) {
  var parent: Option[TreeNode] = None
  var refID = "DUMMY"
  
  def setParent(p: TreeNode) {parent = Some(p)}
  def up: Option[TreeNode] = parent

  override def toString: String = {
    label + "(" + children.map(_.toString).mkString(",") + ")"
  }

  // 根からの全SubPath集合を取得するための関数
  def genSPs (t: TreeNode): ArrayBuffer[SubPath] = {
    val sps = ((t.children).foldLeft(ArrayBuffer[SubPath]()){(p,q) => p ++ genSPs(q)}.map{p => p add (t.label); p}) += SubPath(ArrayBuffer(t.label))
    return sps
  }

  // Suffix ArrayとLCP Arrayを生成するための関数
  def genESA(sps: ArrayBuffer[SubPath]): (ArrayBuffer[Int], ArrayBuffer[Int]) = {
    var suffix_array = sps.sortWith{(a,b) => a <= b}
    var sa = new ArrayBuffer[Int]

    // index 1 からスタートして、  suffix_array(0):  SubPath(ArrayBuffer(0))は除く
    for(i <- 2 until suffix_array.length){
      if(suffix_array(i).last == 1){ sa += 1 }
      else sa += 2
    }

    var lcp_array = new ArrayBuffer[Int]

    // index 1 からスタートして、  suffix_array(0):  SubPath(ArrayBuffer(0))は除く
    for(i <- 2 until (suffix_array.length-1)){
      var lcp = calcLCP(suffix_array(i), suffix_array(i+1))
      lcp_array += lcp
    }
    lcp_array += Int.MaxValue
    return (sa, lcp_array)
  }

  // LCPを計算するための、２つの文字列のsuffixの一致数を返す関数
  def calcLCP (sa1: SubPath, sa2: SubPath): Int = {
    if(sa1.size == 0) return 0
    if(sa2.size == 0) return 0
    var lcp = 0
    while(lcp < sa1.size && sa1.element(lcp) == sa2.element(lcp)){
        lcp += 1
    }
    return lcp
  }

  // 2つのTreeNodeから、ESA(Enhanced Suffix Array)を生成する関数
  def make(y: TreeNode): (ArrayBuffer[Int], ArrayBuffer[Int]) = {

    val r1 = TreeNode(1, ArrayBuffer(this))
    val r2 = TreeNode(2, ArrayBuffer(y))
    this.setParent(r1)
    y.setParent(r2)

    var sps1 = genSPs(r1)
    var sps2 = genSPs(r2)
    val sps = sps1 ++ sps2

    val esa: (ArrayBuffer[Int], ArrayBuffer[Int]) = genESA(sps)
	esa
  }
}

//Rapid
class PTK(decay: Double) {

  def w(n: Int): Double = {
    if(decay == 1.0) return n
    return (math.pow(decay, n + 1) - 1)/(decay - 1) - 1
  }

  var kernel = 0.0 // 最後に計算したカーネル値を保持する変数

  var sa = ArrayBuffer[Int]()
  var lcp = ArrayBuffer[Int]()
  var size = sa.size
  var start = 0

  def find_first_minimal_subtree: Array[Int] = {

    val ls = lcp(start)

    if(start == size - 1)
      return Array(2 - sa(start), sa(start) - 1, start, lcp(start))

    if(start > 0 && lcp(start - 1) >= ls)
      return Array(2 - sa(start), sa(start) - 1, start, lcp(start))

    var end = start
    var le = ls

    while(le == ls) {
      end += 1
      le = lcp(end)
    }

    if(le > ls) {
      val sum = (start until end).foldLeft(0)((x, y) => x + sa(y))
      val c1 = 2 * (end - start) - sum
      val c2 = sum - (end - start)
      kernel += w(ls) * c1 * c2
      return Array(c1, c2, end-1, ls)
    } else { // le < ls
      val sum = (start to end).foldLeft(0)((x, y) => x + sa(y))
      val c1 = 2 * (end - start + 1) - sum
      val c2 = sum - (end - start + 1)
      kernel += w(ls) * c1 * c2
      return Array(c1, c2, end, le)
    }
  }

  def kernel_body(sa: ArrayBuffer[Int], lcp: ArrayBuffer[Int]): Double = {

    kernel = 0.0
    size = sa.size
    start = 0

    val stack = Stack[Array[Int]](find_first_minimal_subtree)
    //println(stack.map(_.mkString("(",",",")")).mkString(">")+"+"+kernel)
    start = stack.top(2) + 1

    while(start < size) {
      var Array(c1, c2, e, l) = find_first_minimal_subtree
      var Array(c1_prev, c2_prev, e_prev, l_prev) = stack.top
      while((l <= l_prev || e == size - 1) && stack.size > 0) {
        kernel += w(l_prev) * (c1 * c2_prev + c2 * c1_prev)
        c1 += c1_prev
        c2 += c2_prev
        stack.pop
        //println(stack.map(_.mkString("(",",",")")).mkString(">")+"+"+kernel)
        if(stack.size > 0) {
          val tmp = stack.top
          c1_prev = tmp(0)
          c2_prev = tmp(1)
          e_prev  = tmp(2)
          l_prev  = tmp(3)
        }
      }
      stack.push(Array(c1, c2, e, l))
      //println(stack.map(_.mkString("(",",",")")).mkString(">")+"+"+kernel)
      start = e + 1
    }
    kernel
  }

  def eval(s: TreeNode, t: TreeNode): Double = {
    val temp = s make t
    sa = temp._1
    lcp = temp._2
	val result = kernel_body(sa, lcp)
	result
  }
}


/*
 * クラス名: htmlSanitizer
 */
class  htmlSanitizer(convertMap: HashMap[String, Int], addFlag: Boolean) {

  /*
   * メソッド名: sanitizeJsoup
   */
  def sanitizeJsoup(htmlFile: File): Option[String] = {
    // 変換結果
    val resultStr = new StringBuilder

    // Jsoup(HTMLパーサー)でDOMオブジェクトを生成
    val document: Document = Jsoup.parse(htmlFile, "UTF-8")
	//Jsoup.parse(htmlSource, "UTF-8")

     // htmlノードを取得
    val htmlElements: Elements = document.getElementsByTag("html")
    if(htmlElements.size() > 1) {
      return None
    }
    val htmlElement = htmlElements.first()

   // DOMオブジェクトから木構造データを生成
    val treeStr = convertToTree(htmlElement)
    treeStr match {
      case Some(x) => resultStr.append(x)
      case None => // Do nothing
    }
    return Some(resultStr.toString())
  }

  /*
   * メソッド名: convertToTree
   * 説明：	DOMオブジェクトから木構造データを生成する
   * 			出力するタグは小文字、または数値変換されて出力される。
   */
  def convertToTree(element: Element): Option[String] = {
    // 変換結果
//--Stack Overflowを避けるため、StringBuilderを使わない
//  var treeStr = new StringBuilder
    var treeStr = ""
    var tag = ""

    // 自ノードのタグ名を出力
    if(convertMap.isEmpty) {
      // タグを変換しない
      tag = element.tagName().toLowerCase()
    } else {
      // タグを数値に変換する
      val cnvStr = tagToNum(element.tagName().toLowerCase(), this.addFlag)
      cnvStr match {
        case Some(x) =>
          tag = x
        case None =>
          // 数値変換不可 -> タグを出力しない
      }
    }
    if(!tag.isEmpty()) {
      // タグとタグ開始マークを出力
      treeStr += tag
      treeStr += """("""
    }

    // 子ノードに対する処理
    val children: Elements = element.children()
    for(childIdx <- 0 to children.size() - 1) {
      if(childIdx > 0) {
        // 2番目以降の子ノード（兄弟ノード）：区切り文字を出力
        treeStr += ""","""
      }
      val childTree = convertToTree(children.get(childIdx))
      childTree match {
        case Some(x) =>
          // 子ノードを出力
          treeStr += x
        case None => // Do nothing
      }
    }

    if(!tag.isEmpty()) {
      // タグ終了マークを出力
      treeStr += """)"""
    }
    return Some(treeStr)
  }

  /*
   * メソッド名: tagToNum
   * 説明：	タグ・数値変換データにより指定されたタグ名を数値データに変換する。
   * 			タグが変換データに存在しない場合は、
   * 			addFlag=true: データに新規登録を行う。
   * 			addFlag=false: 変換不可
   */
  def tagToNum(tag: String, addFlag: Boolean): Option[String] = {
    if(convertMap.isEmpty) {
      // タグ・変換データなし --> 変換不可
      return None
    } else {
      convertMap.get(tag) match {
        case Some(x) =>
          // 数値データを返却
          return Some(x.toString())
        case None =>
          if(addFlag) {
            // 新規データ登録
            //-- map内のvalueの最大値 + 1を新しいvalueとする
            val newNum = convertMap.valuesIterator.max + 1
            convertMap += (tag -> newNum)
            return Some(newNum.toString())
          } else {
            // 変換不可
            return None
          }
      }
    }
  }

  /*
   * メソッド名: getConvertMap
   * 説明：	タグ・数値変換データをCSV形式で取得する
   */
  def getConvertMap(): Option[String] = {
    val mapStr = new StringBuilder

    if(convertMap.isEmpty) {
      return None
    }
    // HashMapを値でソート
    val seq = convertMap.toSeq.sortBy(_._2)
    for(idx <- 0 to seq.size - 1) {
      mapStr.append(seq(idx)._1)
      mapStr.append(",")
      mapStr.append(seq(idx)._2.toString)
      // 改行コード
      mapStr.append(System.getProperty("line.separator"))
    }
    return Some(mapStr.toString())
  }
}

object Html{
    def getSource(url: String): List[String] = {
        val src = Source.fromURL(url, "ISO-8859-1").getLines.toList

        var charset: String = null
        val regex = new Regex("""charset[ ]*=[ ]*[0-9a-z|\-|_]+""")
        for(line <- src){
            val lower = line.toLowerCase
            if(lower.contains("content") && lower.contains("charset")){
                charset = regex.findFirstIn(lower).get
                charset = charset.split("=")(1).trim
            }
        }
		if(charset == null) charset = "utf-8"
        return Source.fromURL(url, charset).getLines.toList
    }
}