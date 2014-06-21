package meta_heuristics.util

import java.io._
import com.typesafe.scalalogging.slf4j.StrictLogging

object SerializeHelper extends StrictLogging
{

   def serializeToFile(obj: Any, filename: String): Unit =
   {
      try {
         val fileOut = new FileOutputStream(filename)
         val out = new ObjectOutputStream(fileOut)
         out.writeObject(obj)
         out.close()
         fileOut.close()
      } catch {
         case i: IOException =>
            i.printStackTrace()
      }


      logger.info("Finished Serializing")
   }

   def deserializeFromFile(filename: String): Any =
   {

      var obj: Any = null
      try {
         val fileIn = new FileInputStream(filename)
         val in = new ObjectInputStream(fileIn)
         obj = in.readObject()
         in.close()
         fileIn.close()
      } catch {
         case i: IOException =>
            i.printStackTrace()
            throw new RuntimeException("io exception")
         case c: ClassNotFoundException =>
            logger.info("Class not found")
            c.printStackTrace()
      }
      logger.info("Finished deserializing")
      obj
   }
}