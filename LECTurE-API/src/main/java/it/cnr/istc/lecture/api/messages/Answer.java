/*
 * Copyright (C) 2017 Riccardo De Benedictis <riccardo.debenedictis@istc.cnr.it>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package it.cnr.istc.lecture.api.messages;

/**
 * This message is intended to inform a teacher that a student has answered a
 * question. Since the question was dispatched to a specific student, it is
 * possible to retrieve the student who gave the answer by the id of the
 * question.
 *
 * @author Riccardo De Benedictis
 */
public class Answer extends Message {

    private final long lessonId;
    private final int questionId;
    private final int answer;

    public Answer(long lessonId, int questionId, int answer) {
        super(MessageType.Answer);
        this.lessonId = lessonId;
        this.questionId = questionId;
        this.answer = answer;
    }

    /**
     * Returns the id of the lesson whose question has been answered.
     *
     * @return a {@code long} representing the id of the lesson whose question
     * has been answered.
     */
    public long getLessonId() {
        return lessonId;
    }

    /**
     * Represents the id of the question whose answer has been given.
     *
     * @return an {@code int} representing the id of the question whose answer
     * has been given.
     */
    public int getQuestionId() {
        return questionId;
    }

    /**
     * Represents the given answer.
     *
     * @return an {@code int} representing the given answer.
     */
    public int getAnswer() {
        return answer;
    }
}
