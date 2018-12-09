// https://adventofcode.com/2018/day/9 in Kotlin
// Try it here: https://try.kotlinlang.org
// (Set execution mode to "JavaScript" — it runs out of memory on "JVM".)

class Marble(val points: Int) {
    var left: Marble = this
    var right: Marble = this

    // Insert a marble to the right of this one.
    fun insert_right(points: Int) {
        val old_right = right
        right = Marble(points)
        right.left = this
        right.right = old_right
        old_right.left = right
    }
    
    // Remove this marble from the cycle and return its point value.
    fun pop(): Int {
        left.right = right
        right.left = left
        return points
    }
}

class Game(val players: Int, val last_marble: Int) {
    var score: Array<Long> = Array(players) { 0L }
    var current: Marble = Marble(0)
    var whose_turn: Int = 0
    var marble_value: Int = 1
    
    // Get the next available marble's value.
    fun next_marble_value() = marble_value++
    
    // Check if the game has ended.
    fun game_over() = marble_value == last_marble
    
    // Award points to the current player.
    fun award_points(points: Int) {
        score[whose_turn] += points.toLong()
    }
    
    // Advance the turn to the next player.
    fun advance_turn() {
        whose_turn = (whose_turn + 1) % players
    }
    
    // Simulate a turn of the game.
    fun turn() {
        val m = next_marble_value()
        if (m % 23 == 0) {
            current = current.left.left.left.left.left.left.left
            val taken = current.pop()
            current = current.right
            award_points(taken + m)
        } else {
            current = current.right
            current.insert_right(m)
            current = current.right
        }
        advance_turn()
    }
    
    // Simulate the whole game and return the final scores.
    fun simulate(): Array<Long> {
        while (!game_over()) turn()
        return score
    }
}

fun main() {
    println(Game(459, 71320).simulate().max())
    println(Game(459, 7132000).simulate().max())
}
