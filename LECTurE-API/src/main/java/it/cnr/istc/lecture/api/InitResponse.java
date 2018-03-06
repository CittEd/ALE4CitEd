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
import java.util.Collections;

/**
 *
 * @author Riccardo De Benedictis
 */
public class InitResponse {

    private final User user;
    private final Collection<LessonModel> models;
    private final Collection<Lesson> following_lessons;
    private final Collection<User> students;
    private final Collection<Lesson> followed_lessons;
    private final Collection<User> teachers;

    public InitResponse(User user, Collection<LessonModel> models, Collection<Lesson> following_lessons, Collection<User> students, Collection<Lesson> followed_lessons, Collection<User> teachers) {
        this.user = user;
        this.models = models;
        this.following_lessons = following_lessons;
        this.students = students;
        this.followed_lessons = followed_lessons;
        this.teachers = teachers;
    }

    public User getUser() {
        return user;
    }

    public Collection<LessonModel> getModels() {
        return Collections.unmodifiableCollection(models);
    }

    public Collection<Lesson> getFollowingLessons() {
        return Collections.unmodifiableCollection(following_lessons);
    }

    public Collection<User> getStudents() {
        return Collections.unmodifiableCollection(students);
    }

    public Collection<Lesson> getFollowedLessons() {
        return Collections.unmodifiableCollection(followed_lessons);
    }

    public Collection<User> getTeachers() {
        return Collections.unmodifiableCollection(teachers);
    }
}
