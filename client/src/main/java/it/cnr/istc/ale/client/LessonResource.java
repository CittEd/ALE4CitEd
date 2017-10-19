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
package it.cnr.istc.ale.client;

import it.cnr.istc.ale.api.Lesson;
import it.cnr.istc.ale.api.LessonAPI;
import it.cnr.istc.ale.api.model.LessonModel;
import java.util.Collection;
import java.util.Map;
import javax.ws.rs.client.Client;
import javax.ws.rs.core.GenericType;
import javax.ws.rs.core.MediaType;

/**
 *
 * @author Riccardo De Benedictis <riccardo.debenedictis@istc.cnr.it>
 */
public class LessonResource implements LessonAPI {

    private final Client client;
    private final String rest_uri;

    public LessonResource(Client client) {
        this.client = client;
        this.rest_uri = "http://" + Context.getContext().getHost() + ":" + Context.getContext().getServicePort();
    }

    @Override
    public Lesson new_lesson(long user_id, String lesson_name, LessonModel model, Map<String, Long> roles) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public Collection<Lesson> get_lessons(long user_id) {
        return client.target(rest_uri)
                .path("lessons")
                .path("get_lessons")
                .queryParam("user_id", user_id)
                .request(MediaType.APPLICATION_JSON)
                .get(new GenericType<Collection<Lesson>>() {
                });
    }

    @Override
    public Collection<Lesson> get_followed_lessons(long user_id) {
        return client.target(rest_uri)
                .path("lessons")
                .path("get_followed_lessons")
                .queryParam("user_id", user_id)
                .request(MediaType.APPLICATION_JSON)
                .get(new GenericType<Collection<Lesson>>() {
                });
    }

    @Override
    public void start_lesson(long lesson_id) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public void pause_lesson(long lesson_id) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public void stop_lesson(long lesson_id) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public void go_at(long lesson_id, long timestamp) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
}
