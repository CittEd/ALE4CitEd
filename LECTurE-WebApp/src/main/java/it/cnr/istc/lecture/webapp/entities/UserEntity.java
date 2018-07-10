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
package it.cnr.istc.lecture.webapp.entities;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import javax.persistence.Column;
import javax.persistence.ElementCollection;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.ManyToMany;
import javax.persistence.OneToMany;

/**
 *
 * @author Riccardo De Benedictis
 */
@Entity
public class UserEntity implements Serializable {

    private static final long serialVersionUID = 1L;
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long id;
    @Column(unique = true)
    private String email;
    private String password;
    private String first_name;
    private String last_name;
    private int concrete;
    private int president;
    private int structurer;
    private int ingenious;
    private int explorer;
    private int evaluator;
    private int worker;
    private int objectivist;
    @ElementCollection
    private final Collection<String> interests = new ArrayList<>();
    @ManyToMany
    private final Collection<UserEntity> teachers = new ArrayList<>();
    @ManyToMany(mappedBy = "teachers")
    private final Collection<UserEntity> students = new ArrayList<>();
    @OneToMany(mappedBy = "teacher")
    private final Collection<LessonEntity> lessons = new ArrayList<>();
    @ManyToMany(mappedBy = "students")
    private final List<LessonEntity> followedLessons = new ArrayList<>();
    @OneToMany
    private final Collection<LessonModelEntity> models = new ArrayList<>();

    public Long getId() {
        return id;
    }

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public String getFirstName() {
        return first_name;
    }

    public void setFirstName(String first_name) {
        this.first_name = first_name;
    }

    public String getLastName() {
        return last_name;
    }

    public void setLastName(String last_name) {
        this.last_name = last_name;
    }

    public int getConcrete() {
        return concrete;
    }

    public void setConcrete(int concrete) {
        this.concrete = concrete;
    }

    public int getPresident() {
        return president;
    }

    public void setPresident(int president) {
        this.president = president;
    }

    public int getStructurer() {
        return structurer;
    }

    public void setStructurer(int structurer) {
        this.structurer = structurer;
    }

    public int getIngenious() {
        return ingenious;
    }

    public void setIngenious(int ingenious) {
        this.ingenious = ingenious;
    }

    public int getExplorer() {
        return explorer;
    }

    public void setExplorer(int explorer) {
        this.explorer = explorer;
    }

    public int getEvaluator() {
        return evaluator;
    }

    public void setEvaluator(int evaluator) {
        this.evaluator = evaluator;
    }

    public int getWorker() {
        return worker;
    }

    public void setWorker(int worker) {
        this.worker = worker;
    }

    public int getObjectivist() {
        return objectivist;
    }

    public void setObjectivist(int objectivist) {
        this.objectivist = objectivist;
    }

    public Collection<String> getInterests() {
        return Collections.unmodifiableCollection(interests);
    }

    public void addInterest(String interest) {
        interests.add(interest);
    }

    public void removeInterest(String interest) {
        interests.remove(interest);
    }

    public Collection<UserEntity> getTeachers() {
        return Collections.unmodifiableCollection(teachers);
    }

    public void addTeacher(UserEntity teacher) {
        teachers.add(teacher);
    }

    public void removeTeacher(UserEntity teacher) {
        teachers.remove(teacher);
    }

    public Collection<UserEntity> getStudents() {
        return Collections.unmodifiableCollection(students);
    }

    public void addStudent(UserEntity student) {
        students.add(student);
    }

    public void removeStudent(UserEntity student) {
        students.remove(student);
    }

    public Collection<LessonEntity> getLessons() {
        return Collections.unmodifiableCollection(lessons);
    }

    public void addLesson(LessonEntity lesson) {
        lessons.add(lesson);
    }

    public void removeLesson(LessonEntity lesson) {
        lessons.remove(lesson);
    }

    public Collection<LessonEntity> getFollowedLessons() {
        return Collections.unmodifiableCollection(followedLessons);
    }

    public void addFollowedLesson(LessonEntity role) {
        followedLessons.add(role);
    }

    public void removeFollowedLesson(LessonEntity role) {
        followedLessons.remove(role);
    }

    public Collection<LessonModelEntity> getModels() {
        return Collections.unmodifiableCollection(models);
    }

    public void addModel(LessonModelEntity model) {
        models.add(model);
    }

    public void removeModel(LessonModelEntity model) {
        models.remove(model);
    }
}
