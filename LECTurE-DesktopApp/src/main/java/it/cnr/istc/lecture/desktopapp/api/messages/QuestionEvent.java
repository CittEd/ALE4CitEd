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
package it.cnr.istc.lecture.desktopapp.api.messages;

import java.util.Collections;
import java.util.List;
import javax.json.bind.annotation.JsonbCreator;
import javax.json.bind.annotation.JsonbProperty;

/**
 *
 * @author Riccardo De Benedictis
 */
public class QuestionEvent extends Event {

    private final String question;
    private final List<String> answers;
    private Integer answer;

    @JsonbCreator
    public QuestionEvent(@JsonbProperty("lessonId") long lesson_id, @JsonbProperty("eventId") int event_id, @JsonbProperty("role") String role, @JsonbProperty("question") String question, @JsonbProperty("answers") List<String> answers, @JsonbProperty("answer") Integer answer) {
        super(EventType.QuestionEvent, lesson_id, event_id, role);
        this.question = question;
        this.answers = answers;
        this.answer = answer;
    }

    public String getQuestion() {
        return question;
    }

    public List<String> getAnswers() {
        return Collections.unmodifiableList(answers);
    }

    public Integer getAnswer() {
        return answer;
    }

    public void setAnswer(Integer answer) {
        this.answer = answer;
    }
}
