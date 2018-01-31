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
package it.cnr.istc.ale.api.messages;

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
    private final Collection<String> answers;

    @JsonCreator
    public QuestionEvent(@JsonProperty("id") long id, @JsonProperty("lesson_id") long lesson_id, @JsonProperty("question") String question, @JsonProperty("answers") Collection<String> answers) {
        super(id, lesson_id);
        this.question = question;
        this.answers = answers;
    }

    public String getQuestion() {
        return question;
    }

    public Collection<String> getAnswers() {
        return Collections.unmodifiableCollection(answers);
    }
}
