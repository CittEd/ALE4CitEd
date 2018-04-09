/*
 * Copyright (C) 2018 Riccardo De Benedictis
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
package it.cnr.istc.lecture.api;

import it.cnr.istc.lecture.api.model.LessonModel;
import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author Riccardo De Benedictis
 */
public class NewLessonRequest {

    public long teacher_id;
    public String lesson_name;
    public Long lesson_model_id;
    public LessonModel model;
    public HashMap<String, Long> roles;

    public NewLessonRequest() {
    }

    public NewLessonRequest(long teacher_id, String lesson_name, Long lesson_model_id, Map<String, Long> roles) {
        this.teacher_id = teacher_id;
        this.lesson_name = lesson_name;
        this.lesson_model_id = lesson_model_id;
        this.roles = new HashMap<>(roles);
    }

    public NewLessonRequest(long teacher_id, String lesson_name, LessonModel model, Map<String, Long> roles) {
        this.teacher_id = teacher_id;
        this.lesson_name = lesson_name;
        this.model = model;
        this.roles = new HashMap<>(roles);
    }
}
