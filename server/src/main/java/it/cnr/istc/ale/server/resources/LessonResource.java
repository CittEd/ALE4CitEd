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
package it.cnr.istc.ale.server.resources;

import com.fasterxml.jackson.core.type.TypeReference;
import it.cnr.istc.ale.api.Lesson;
import it.cnr.istc.ale.api.LessonAPI;
import it.cnr.istc.ale.api.model.LessonModel;
import it.cnr.istc.ale.server.Context;
import java.io.IOException;
import java.util.Collection;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;

/**
 *
 * @author Riccardo De Benedictis <riccardo.debenedictis@istc.cnr.it>
 */
@Path("lessons")
public class LessonResource implements LessonAPI {

    private static final Logger LOG = Logger.getLogger(LessonResource.class.getName());

    @Override
    @POST
    @Path("new_lesson")
    @Produces(MediaType.APPLICATION_JSON)
    public Lesson new_lesson(@FormParam("teacher_id") long teacher_id, @FormParam("lesson_name") String lesson_name, @FormParam("model") String model, @FormParam("roles") String roles) {
        try {
            return Context.getContext().new_lesson(teacher_id, lesson_name, Context.MAPPER.readValue(model, LessonModel.class), Context.MAPPER.readValue(roles, new TypeReference<Map<String, Long>>() {
            }));
        } catch (IOException ex) {
            LOG.log(Level.SEVERE, null, ex);
            return null;
        }
    }

    @Override
    @GET
    @Path("get_lessons")
    @Produces(MediaType.APPLICATION_JSON)
    public Collection<Lesson> get_lessons(@QueryParam("teacher_id") long teacher_id) {
        return Context.getContext().get_lessons(teacher_id);
    }

    @Override
    @GET
    @Path("get_followed_lessons")
    @Produces(MediaType.APPLICATION_JSON)
    public Collection<Lesson> get_followed_lessons(@QueryParam("student_id") long student_id) {
        return Context.getContext().get_followed_lessons(student_id);
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
