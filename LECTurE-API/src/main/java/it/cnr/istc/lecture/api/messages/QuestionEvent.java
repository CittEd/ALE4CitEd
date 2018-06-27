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

import java.util.Collection;
import java.util.List;

/**
 *
 * @author Riccardo De Benedictis
 */
public class QuestionEvent extends Event {

    public String question;
    public List<String> answers;
    public Integer answer;

    public QuestionEvent() {
    }

    public QuestionEvent(long lesson_id, int event_id, Collection<Long> targets, long time, String question, List<String> answers, Integer answer) {
        super(EventType.QuestionEvent, lesson_id, event_id, targets, time);
        this.question = question;
        this.answers = answers;
        this.answer = answer;
    }
}
