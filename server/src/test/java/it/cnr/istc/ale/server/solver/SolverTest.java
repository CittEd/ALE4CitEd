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
package it.cnr.istc.ale.server.solver;

import com.fasterxml.jackson.core.JsonProcessingException;
import it.cnr.istc.ale.api.model.LessonModel;
import it.cnr.istc.ale.api.model.QuestionEventTemplate;
import it.cnr.istc.ale.api.model.Relation;
import it.cnr.istc.ale.api.model.TextEventTemplate;
import it.cnr.istc.ale.server.Context;
import java.util.Arrays;
import java.util.Collections;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.junit.Test;

/**
 *
 * @author Riccardo De Benedictis
 */
public class SolverTest {

    /**
     * Test of getValue method, of class Solver.
     */
    @Test
    public void testGetValue() {
        TemporalNetwork s = new TemporalNetwork();
        int tp0 = s.newVar();
        int tp1 = s.newVar();
        int tp2 = s.newVar();

        s.newTemporalRelation(tp0, tp1, 10, 20);
        s.newTemporalRelation(tp1, tp2, 10, 20);

        s.propagate();

        double tp0_val = s.getValue(tp0);
        double tp1_val = s.getValue(tp1);
        double tp2_val = s.getValue(tp2);

        s.setValue(tp0, 10);

        tp0_val = s.getValue(tp0);
        tp1_val = s.getValue(tp1);
        tp2_val = s.getValue(tp2);

        LessonModel lm = new LessonModel("Lesson one", Arrays.asList(
                "Police officer",
                "Firefighter"
        ), Arrays.asList(new TextEventTemplate("police_0", "Police officer", null, null, Collections.emptyList(), Collections.emptyList(), "Hi police officer!\nThis is your first text event."),
                new QuestionEventTemplate("police_1", "Police officer", null, null, Collections.emptyList(), Collections.emptyList(), "This is your first question! Which is your answer?", Arrays.asList(new QuestionEventTemplate.Answer("First choice", "police_1"))),
                new TextEventTemplate("police_2", "Police officer", null, null, Collections.emptyList(), Collections.emptyList(), "This is your second event."),
                new TextEventTemplate("fire_0", "Firefighter", null, null, Collections.emptyList(), Collections.emptyList(), "Hi firefighter!\nThis is your first text event."),
                new TextEventTemplate("fire_1", "Firefighter", null, null, Collections.emptyList(), Collections.emptyList(), "This is your second event.")
        ), Arrays.asList(
                "police_0", "police_1", "fire_0"
        ), Arrays.asList(
                new Relation("this", "police_0", 5l, 10l, TimeUnit.SECONDS),
                new Relation("police_0", "police_1", 5l, 10l, TimeUnit.SECONDS),
                new Relation("this", "fire_0", 5l, 10l, TimeUnit.SECONDS)
        ));
        try {
            System.out.println(Context.MAPPER.writeValueAsString(lm));
        } catch (JsonProcessingException ex) {
            Logger.getLogger(SolverTest.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
}
