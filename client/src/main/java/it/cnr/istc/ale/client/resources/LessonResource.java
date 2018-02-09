/*
 * Copyright (C) 2018 Riccardo De Benedictis <riccardo.debenedictis@istc.cnr.it>
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
package it.cnr.istc.ale.client.resources;

import it.cnr.istc.ale.api.Lesson;
import it.cnr.istc.ale.api.LessonAPI;
import it.cnr.istc.ale.api.messages.Event;
import it.cnr.istc.ale.api.messages.Token;
import it.cnr.istc.ale.api.model.LessonModel;
import it.cnr.istc.ale.client.Config;
import java.util.Collection;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.Entity;
import javax.ws.rs.core.Form;
import javax.ws.rs.core.GenericType;
import javax.ws.rs.core.MediaType;

/**
 *
 * @author Riccardo De Benedictis
 */
public class LessonResource implements LessonAPI {

    private final Client client;
    private final String rest_uri;

    public LessonResource(Client client) {
        this.client = client;
        this.rest_uri = "http://" + Config.getInstance().getParam(Config.Param.Host) + ":" + Config.getInstance().getParam(Config.Param.ServicePort);
    }

    @Override
    public Lesson new_lesson_by_model(long teacher_id, String lesson_name, String model, String roles) {
        Form form = new Form();
        form.param("teacher_id", Long.toString(teacher_id));
        form.param("lesson_name", lesson_name);
        form.param("model", model);
        form.param("roles", roles);
        return client.target(rest_uri)
                .path("lessons")
                .path("new_lesson_by_model")
                .request(MediaType.APPLICATION_JSON)
                .post(Entity.form(form), Lesson.class);
    }

    @Override
    public Lesson new_lesson_by_model_id(long teacher_id, String lesson_name, long model_id, String roles) {
        Form form = new Form();
        form.param("teacher_id", Long.toString(teacher_id));
        form.param("lesson_name", lesson_name);
        form.param("model_id", Long.toString(model_id));
        form.param("roles", roles);
        return client.target(rest_uri)
                .path("lessons")
                .path("new_lesson_by_model_id")
                .request(MediaType.APPLICATION_JSON)
                .post(Entity.form(form), Lesson.class);
    }

    @Override
    public void remove_lesson(long lesson_id) {
        Form form = new Form();
        form.param("lesson_id", Long.toString(lesson_id));
        client.target(rest_uri)
                .path("lessons")
                .path("remove_lesson")
                .request(MediaType.APPLICATION_JSON)
                .post(Entity.form(form));
    }

    @Override
    public void solve_lesson(long lesson_id) {
        Form form = new Form();
        form.param("lesson_id", Long.toString(lesson_id));
        client.target(rest_uri)
                .path("lessons")
                .path("solve_lesson")
                .request(MediaType.APPLICATION_JSON)
                .put(Entity.form(form));
    }

    @Override
    public Collection<Lesson> get_lessons(long teacher_id) {
        return client.target(rest_uri)
                .path("lessons")
                .path("get_lessons")
                .queryParam("teacher_id", teacher_id)
                .request(MediaType.APPLICATION_JSON)
                .get(new GenericType<Collection<Lesson>>() {
                });
    }

    @Override
    public Collection<Token> get_tokens(long lesson_id) {
        return client.target(rest_uri)
                .path("lessons")
                .path("get_tokens")
                .queryParam("lesson_id", lesson_id)
                .request(MediaType.APPLICATION_JSON)
                .get(new GenericType<Collection<Token>>() {
                });
    }

    @Override
    public Collection<LessonModel> get_models(long teacher_id) {
        return client.target(rest_uri)
                .path("lessons")
                .path("get_models")
                .queryParam("teacher_id", teacher_id)
                .request(MediaType.APPLICATION_JSON)
                .get(new GenericType<Collection<LessonModel>>() {
                });
    }

    @Override
    public Collection<Lesson> get_followed_lessons(long student_id) {
        return client.target(rest_uri)
                .path("lessons")
                .path("get_followed_lessons")
                .queryParam("student_id", student_id)
                .request(MediaType.APPLICATION_JSON)
                .get(new GenericType<Collection<Lesson>>() {
                });
    }

    @Override
    public Collection<Event> get_events(long lesson_id, long student_id) {
        return client.target(rest_uri)
                .path("lessons")
                .path("get_events")
                .queryParam("lesson_id", lesson_id)
                .queryParam("student_id", student_id)
                .request(MediaType.APPLICATION_JSON)
                .get(new GenericType<Collection<Event>>() {
                });
    }

    @Override
    public void start_lesson(long lesson_id) {
        Form form = new Form();
        form.param("lesson_id", Long.toString(lesson_id));
        client.target(rest_uri)
                .path("lessons")
                .path("start_lesson")
                .request(MediaType.APPLICATION_JSON)
                .put(Entity.form(form));
    }

    @Override
    public void pause_lesson(long lesson_id) {
        Form form = new Form();
        form.param("lesson_id", Long.toString(lesson_id));
        client.target(rest_uri)
                .path("lessons")
                .path("pause_lesson")
                .request(MediaType.APPLICATION_JSON)
                .put(Entity.form(form));
    }

    @Override
    public void stop_lesson(long lesson_id) {
        Form form = new Form();
        form.param("lesson_id", Long.toString(lesson_id));
        client.target(rest_uri)
                .path("lessons")
                .path("stop_lesson")
                .request(MediaType.APPLICATION_JSON)
                .put(Entity.form(form));
    }

    @Override
    public void go_at(long lesson_id, long timestamp) {
        Form form = new Form();
        form.param("lesson_id", Long.toString(lesson_id));
        form.param("timestamp", Long.toString(timestamp));
        client.target(rest_uri)
                .path("lessons")
                .path("go_at")
                .request(MediaType.APPLICATION_JSON)
                .put(Entity.form(form));
    }

    @Override
    public void set_time(long lesson_id, int token_id, long timestamp) {
        Form form = new Form();
        form.param("lesson_id", Long.toString(lesson_id));
        form.param("token_id", Integer.toString(token_id));
        form.param("timestamp", Long.toString(timestamp));
        client.target(rest_uri)
                .path("lessons")
                .path("set_time")
                .request(MediaType.APPLICATION_JSON)
                .put(Entity.form(form));
    }

    @Override
    public void answer_question(long lesson_id, int question_id, int answer_id) {
        Form form = new Form();
        form.param("lesson_id", Long.toString(lesson_id));
        form.param("question_id", Integer.toString(question_id));
        form.param("answer_id", Integer.toString(answer_id));
        client.target(rest_uri)
                .path("lessons")
                .path("answer_question")
                .request(MediaType.APPLICATION_JSON)
                .put(Entity.form(form));
    }
}
