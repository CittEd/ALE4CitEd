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
package it.cnr.istc.ale.api;

import it.cnr.istc.ale.api.model.LessonModel;
import java.util.Collection;
import java.util.Map;

/**
 *
 * @author Riccardo De Benedictis <riccardo.debenedictis@istc.cnr.it>
 */
public interface LessonAPI {

    public Lesson new_lesson(long user_id, String lesson_name, LessonModel model, Map<String, Long> roles);

    public Collection<Lesson> get_lessons(long user_id);

    public Collection<Lesson> get_followed_lessons(long user_id);

    public void start_lesson(long lesson_id);

    public void pause_lesson(long lesson_id);

    public void stop_lesson(long lesson_id);

    public void go_at(long lesson_id, long timestamp);
}
