package streams

import org.junit.Assert.assertEquals
import org.junit._

class BloxorzSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) =>
        require(block.isLegal) // The solution must always lead to legal blocks
        move match {
          case Left => block.left
          case Right => block.right
          case Up => block.up
          case Down => block.down
        }
      }
  }

  trait Level1 extends SolutionChecker {
    /* terrain for level 1*/

    val level =
      """ooo-------
        |oSoooo----
        |ooooooooo-
        |-ooooooooo
        |-----ooToo
        |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }


  @Test def `terrain function level 1 (10pts)`: Unit =
    new Level1 {
      assert(terrain(Pos(0, 2)), "0,2")
      assert(terrain(Pos(0, 0)), "0,0")
      assert(terrain(Pos(1, 1)), "1,1") // start
      assert(terrain(Pos(4, 7)), "4,7") // goal
      assert(terrain(Pos(5, 8)), "5,8")
      assert(!terrain(Pos(5, 9)), "5,9")
      assert(terrain(Pos(4, 9)), "4,9")
      assert(!terrain(Pos(6, 8)), "6,8")
      assert(!terrain(Pos(4, 11)), "4,11")
      assert(!terrain(Pos(-1, 0)), "-1,0")
      assert(!terrain(Pos(0, -1)), "0,-1")
    }

  @Test def `find char level 1 (10pts)`: Unit =
    new Level1 {
      assertEquals(Pos(1, 1), startPos)
    }

  @Test def `Bloc inside`: Unit =
    new Level1 {
      assertEquals(true, Block(Pos(0, 0), Pos(0, 0)).isStanding)
      assertEquals(false, Block(Pos(0, 0), Pos(0, 1)).isStanding)
      assertEquals(true, Block(Pos(0, 0), Pos(0, 1)).isLegal)
      assertEquals(false, Block(Pos(0, 2), Pos(0, 3)).isLegal)
    }

  @Test def `neighborsWithHistory`: Unit =
    new Level1 {
      private val value: LazyList[(Block, List[Move])] = neighborsWithHistory(Block(Pos(1, 1), Pos(1, 1)), List(Left, Up))
      private val actal = value.toSet
      private val expect = Set(
        (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))
      )
      assertEquals(expect, actal)
    }

  @Test def `newNeighborsOnly`: Unit =
    new Level1 {
      private val actal: LazyList[(Block, List[Move])] = newNeighborsOnly(
        Set(
          (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
          (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))
        ).to(LazyList),

        Set(Block(Pos(1, 2), Pos(1, 3)), Block(Pos(1, 1), Pos(1, 1)))
      )
      private val expect: LazyList[(Block, List[Move])] = Set(
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))
      ).to(LazyList)
      assertEquals(expect, actal)
    }

  @Test def `Move block`: Unit =
    new Level1 {
      private val block: Block = Block(startPos, startPos)
      private val neighbors: List[(Block, Move)] = block.neighbors
      private val neighbors1: List[(Block, Move)] = block.legalNeighbors
      println(neighbors.size)
      println(neighbors1.size)
    }


  @Test def `optimal solution for level 1 (5pts)`: Unit =
    new Level1 {
      assertEquals(Block(goal, goal), solve(solution))
    }



  @Test def `optimal solution length for level 1 (5pts)`: Unit =
    new Level1 {
      assertEquals(optsolution.length, solution.length)
    }

  @Test def `SORT`: Unit =
    new Level1 {
      List(1,2,3,4,5,6).sortBy()(Ordering.Int)
    }


  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000 * 1000)
}
