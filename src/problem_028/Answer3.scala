package problem_028

import common._

object Answer3 extends Answer {

    override def title = "Using formula."

    def answer = (1 /: (3 to 1001 by 2)) { (sum, scale) =>
        // top-right corner:    scale^2                -> scale * scale
        // top-left corner:     scale^2 - (scale - 1)  -> scale * scale - scale + 1
        // bottom-left corner:  scale^2 - 2(scale - 1) -> scale * scale - 2 * scale + 2
        // bottom-right corner: scale^2 - 3(scale - 1) -> scale * scale - 3 * scale + 3
        // sum of them: 4 * scale * scale - 6 * scale + 6
        //     -> ((scale * (2 * scale - 3) + 3) << 1)
        sum + ((scale * (2 * scale - 3) + 3) << 1)
    }

}