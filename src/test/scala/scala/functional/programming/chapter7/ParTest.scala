package scala.functional.programming.chapter7

import java.util.concurrent.{LinkedBlockingDeque, ThreadPoolExecutor, TimeUnit}

import org.junit.Assert.assertEquals
import org.junit.Test

class ParTest {

  @Test
  def testParagraphCount(): Unit = {
    val paragraphs = List(
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nunc suscipit id ipsum vel dictum. In hac habitasse platea dictumst. Interdum et malesuada fames ac ante ipsum primis in faucibus. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Mauris quis condimentum dui, a suscipit velit. Integer finibus, leo non tincidunt elementum, erat ligula aliquet erat, a ultricies sapien turpis ut lacus. In imperdiet eu libero vel maximus. Nulla sed ante velit. Praesent interdum justo ipsum, et placerat ligula auctor a. Sed blandit velit et risus convallis, eget blandit tellus lacinia.",
      "Donec finibus at purus quis posuere. Nulla tristique neque justo, hendrerit euismod arcu faucibus eu. Aenean lacinia mauris semper risus condimentum scelerisque. Duis vitae commodo tortor. Donec varius mauris a volutpat volutpat. Aenean aliquet volutpat ante ac aliquam. Orci varius natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Aliquam tincidunt purus dolor, ut semper purus sollicitudin eu.",
      "Cras placerat bibendum diam, a tempus dui aliquam sit amet. Nunc ut justo sed nulla mattis tempus. Nam tincidunt risus scelerisque, fermentum magna id, gravida nulla. Vestibulum euismod ante eget nulla convallis, in pulvinar lacus convallis. Nam vitae rhoncus ligula. In dictum pretium libero a bibendum. Praesent at malesuada turpis. Sed tortor nisi, semper in nisl ut, auctor malesuada mi. Aliquam efficitur rutrum neque, ac bibendum lorem dictum et. Mauris vitae mollis justo, a scelerisque sem. Morbi interdum, mi semper consectetur viverra, arcu velit tristique erat, eu ullamcorper enim justo sed nisl. Pellentesque vulputate dui ac ultricies facilisis.",
      "Nulla convallis turpis leo, nec finibus mi semper sed. Pellentesque hendrerit dignissim convallis. Curabitur eget fringilla lectus, id semper nisi. Nulla tempus quis nibh a hendrerit. Nulla posuere vehicula faucibus. Aliquam non lobortis enim. In ultrices dolor quis porta porta. Aliquam maximus et ipsum sed fermentum. Pellentesque ultricies condimentum lectus mollis efficitur.",
      "Quisque sit amet nulla erat. Praesent vitae scelerisque nisi, eget auctor diam. Cras varius neque dui, pulvinar faucibus sem placerat eu. Cras vestibulum eu orci nec laoreet. Duis urna nunc, venenatis vel convallis nec, maximus posuere est. Donec id sollicitudin risus, vel ornare turpis. Nullam eget placerat sem. Proin ut augue in nisi vehicula porttitor. Integer maximus eu turpis nec venenatis. Pellentesque in bibendum erat, vitae auctor quam. Duis ullamcorper dictum purus vel blandit. Phasellus sit amet neque erat. Pellentesque imperdiet fermentum quam a convallis. Nam tincidunt lacus eu arcu vestibulum, ac maximus nunc bibendum. Fusce sit amet orci quis augue tincidunt aliquam in ut dui. Vestibulum sit amet hendrerit orci."
    )
    val es = new ThreadPoolExecutor(0, 4, Long.MaxValue, TimeUnit.NANOSECONDS, new LinkedBlockingDeque())
    val plan = Par.sequence(paragraphs.map(lines => Par.lazyUnit(lines))
      .map(line => Par.map(line)(_.split(" ").length)))(es).get()
    val count = Par.reduce(plan)(_ + _)(es).get()
    assertEquals(414, count)
  }

  @Test
  def maxIntTest(): Unit = {
    val es = new ThreadPoolExecutor(0, 4, Long.MaxValue, TimeUnit.NANOSECONDS, new LinkedBlockingDeque())
    val ints = List(11, 89, 64, 100, 64, 68, 45, 72, 50, 39, 75, 16, 23, 49, 98, 95, 93, 19, 53, 51, 71, 55, 39, 17, 28, 6, 22, 1, 48, 65, 11, 27, 39, 50, 68, 63, 21, 76, 19, 5, 89, 87, 37, 63, 13, 7, 5, 32, 40, 37, 96, 96, 66, 68, 59, 89, 82, 18, 73, 21, 68, 1, 55, 42, 28, 84, 97, 14, 52, 54, 21, 59, 8, 38, 65, 51, 41, 30, 46, 53, 90, 47, 15, 83, 39, 17, 12, 58, 53, 63, 24, 78, 16, 53, 26, 89, 80, 30, 72, 16)
    assertEquals(100, Par.reduce(ints)(Integer.max)(es).get())
  }

}
