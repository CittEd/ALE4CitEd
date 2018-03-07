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
import java.util.Collection;

/**
 *
 * @author Riccardo De Benedictis
 */
public class InitResponse {

    public User user;
    /**
     * The lessons followed as a student.
     */
    public Collection<Lesson> following_lessons;
    /**
     * The followed teachers.
     */
    public Collection<User> teachers;
    /**
     * The associated lesson models.
     */
    public Collection<LessonModel> models;
    /**
     * The lessons followed as a teacher.
     */
    public Collection<Lesson> teaching_lessons;
    /**
     * The followed students.
     */
    public Collection<User> students;

    public InitResponse() {
    }

    public InitResponse(User user, Collection<Lesson> following_lessons, Collection<User> teachers, Collection<LessonModel> models, Collection<Lesson> teaching_lessons, Collection<User> students) {
        this.user = user;
        this.following_lessons = following_lessons;
        this.teachers = teachers;
        this.models = models;
        this.teaching_lessons = teaching_lessons;
        this.students = students;
    }
}
