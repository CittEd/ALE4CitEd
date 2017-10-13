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
package it.cnr.istc.ale.api.model;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.Collection;
import java.util.Collections;

/**
 *
 * @author Riccardo De Benedictis <riccardo.debenedictis@istc.cnr.it>
 */
public class QuestionEvent extends Event {

    private final String question;
    private final Collection<Answer> answers;

    @JsonCreator
    public QuestionEvent(@JsonProperty("name") String name, @JsonProperty("role") String role, @JsonProperty("trConditions") Collection<Condition> trConditions, @JsonProperty("exConditions") Collection<Condition> exConditions, @JsonProperty("events") Collection<String> events, @JsonProperty("relations") Collection<Relation> relations, @JsonProperty("question") String question, @JsonProperty("answers") Collection<Answer> answers) {
        super(name, role, trConditions, exConditions, events, relations);
        this.question = question;
        this.answers = answers;
    }

    public String getQuestion() {
        return question;
    }

    public Collection<Answer> getAnswers() {
        return Collections.unmodifiableCollection(answers);
    }

    public class Answer {

        private final String answer;
        private final String event;

        @JsonCreator
        public Answer(@JsonProperty("answer") String answer, @JsonProperty("event") String event) {
            this.answer = answer;
            this.event = event;
        }

        public String getAnswer() {
            return answer;
        }

        public String getEvent() {
            return event;
        }
    }
}
