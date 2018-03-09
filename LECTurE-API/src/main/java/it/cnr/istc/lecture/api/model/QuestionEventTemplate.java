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
package it.cnr.istc.lecture.api.model;

import java.util.Collection;
import java.util.Collections;
import java.util.List;

/**
 *
 * @author Riccardo De Benedictis
 */
public class QuestionEventTemplate extends EventTemplate {

    public String question;
    public List<Answer> answers;

    public QuestionEventTemplate() {
    }

    public QuestionEventTemplate(String name, String role, Condition trigger_condition, Condition execution_condition, Collection<String> events, Collection<Relation> relations, String question, List<Answer> answers) {
        super(EventTemplateType.QuestionEventTemplate, name, role, trigger_condition, execution_condition, events, relations);
        this.question = question;
        this.answers = answers;
    }

    public static class Answer {

        public String answer;
        public String event;

        public Answer() {
        }

        public Answer(String answer, String event) {
            this.answer = answer;
            this.event = event;
        }
    }
}
