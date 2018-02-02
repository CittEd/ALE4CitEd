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

/**
 *
 * @author Riccardo De Benedictis
 */
public interface LessonAPI {

    /**
     * Creates a new lesson given the id of the teacher that wants to follow the
     * lesson, the name of the lesson instance, the model of the lesson and a
     * map of roles containing, for each role, the corresponding student id.
     *
     * @param teacher_id a {@code long} representing the id of a teacher.
     * @param lesson_name a {@code String} representing the name of the specific
     * lesson instance.
     * @param model the model of the lesson.
     * @param roles the mapping of the roles for the lesson instance.
     * @return a {@link Lesson} instance representing a specific lesson.
     */
    public Lesson new_lesson(long teacher_id, String lesson_name, String model, String roles);

    /**
     * Given a teacher id, returns the collection of lessons followed as a
     * teacher.
     *
     * @param teacher_id a {@code long} representing the id of a teacher.
     * @return the collection of lessons followed as a teacher.
     */
    public Collection<Lesson> get_lessons(long teacher_id);

    /**
     * Given a teacher id, returns the lesson models associated to the teacher.
     *
     * @param teacher_id a {@code long} representing the id of a teacher.
     * @return the collection of lessons models associated teacher.
     */
    public Collection<LessonModel> get_models(long teacher_id);

    /**
     * Given a student id, returns the collection of lessons followed as a
     * student.
     *
     * @param student_id a {@code long} representing the id of a student.
     * @return the collection of lessons followed as a student.
     */
    public Collection<Lesson> get_followed_lessons(long student_id);

    /**
     * Starts the execution of the lesson identified by the given lesson id.
     *
     * @param lesson_id a {@code long} representing the id of a lesson.
     */
    public void start_lesson(long lesson_id);

    /**
     * Pauses the execution of the lesson identified by the given lesson id.
     *
     * @param lesson_id a {@code long} representing the id of a lesson.
     */
    public void pause_lesson(long lesson_id);

    /**
     * Stops the execution of the lesson identified by the given lesson id.
     *
     * @param lesson_id a {@code long} representing the id of a lesson.
     */
    public void stop_lesson(long lesson_id);

    /**
     * Moves the temporal execution of the lesson identified by the given lesson
     * id at the specified timestamp.
     *
     * @param lesson_id a {@code long} representing the id of a lesson.
     * @param timestamp a {@code long} representing the execution timestamp.
     */
    public void go_at(long lesson_id, long timestamp);
}
