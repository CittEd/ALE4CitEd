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
package it.cnr.istc.lecture.webapp.api.model;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import javax.json.bind.annotation.JsonbCreator;
import javax.json.bind.annotation.JsonbProperty;

/**
 *
 * @author Riccardo De Benedictis
 */
public class QuestionEventTemplate extends EventTemplate {

    private final String question;
    private final List<Answer> answers;

    @JsonbCreator
    public QuestionEventTemplate(@JsonbProperty("name") String name, @JsonbProperty("role") String role, @JsonbProperty("triggerCondition") Condition trigger_condition, @JsonbProperty("executionCondition") Condition execution_condition, @JsonbProperty("events") Collection<String> events, @JsonbProperty("relations") Collection<Relation> relations, @JsonbProperty("question") String question, @JsonbProperty("answers") List<Answer> answers) {
        super(EventTemplateType.QuestionEventTemplate, name, role, trigger_condition, execution_condition, events, relations);
        this.question = question;
        this.answers = answers;
    }

    public String getQuestion() {
        return question;
    }

    public List<Answer> getAnswers() {
        return Collections.unmodifiableList(answers);
    }

    public static class Answer {

        private final String answer;
        private final String event;

        @JsonbCreator
        public Answer(@JsonbProperty("answer") String answer, @JsonbProperty("event") String event) {
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
