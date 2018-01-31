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

import it.cnr.istc.ale.api.model.Event;
import it.cnr.istc.ale.api.model.LessonModel;
import it.cnr.istc.ale.api.model.Relation;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Deque;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Riccardo De Benedictis <riccardo.debenedictis@istc.cnr.it>
 */
public class LessonManager implements TemporalNetworkListener {

    public static final String THIS = "this";
    private static final Logger LOG = Logger.getLogger(LessonManager.class.getName());
    public final TemporalNetwork network = new TemporalNetwork();
    private final Map<String, Event> event_templates = new HashMap<>();
    private final List<Token> tokens = new ArrayList<>();
    private final Collection<Token> triggerable_tokens = new ArrayList<>();
    private final Deque<Token> prop_q = new ArrayDeque<>();
    public final int origin = network.newVar();
    private final Collection<LessonManagerListener> listeners = new ArrayList<>();

    public LessonManager() {
        tokens.add(new Token(null, origin, null));
    }

    public void setModel(final LessonModel model) {
        for (Event event_template : model.getModel()) {
            if (event_templates.containsKey(event_template.getName())) {
                LOG.log(Level.WARNING, "Renaming event {0}", event_template.getName());
            }
            event_templates.put(event_template.getName(), event_template);
        }
        Map<String, Token> c_tks = new HashMap<>();
        for (String id : model.getEvents()) {
            Token tk = new Token(null, network.newVar(), event_templates.get(id));
            tokens.add(tk);
            for (LessonManagerListener l : listeners) {
                l.newToken(tk);
            }
            c_tks.put(id, tk);
            prop_q.push(tk);
        }
        for (Relation rel : model.getRelations()) {
            double lb = rel.getLb() != null ? rel.getUnit().convert(rel.getLb(), TimeUnit.MILLISECONDS) : Double.NEGATIVE_INFINITY;
            double ub = rel.getUb() != null ? rel.getUnit().convert(rel.getUb(), TimeUnit.MILLISECONDS) : Double.NEGATIVE_INFINITY;
            if (rel.getFrom().equals(THIS)) {
                network.newTemporalRelation(origin, c_tks.get(rel.getTo()).tp, lb, ub);
            } else {
                network.newTemporalRelation(c_tks.get(rel.getFrom()).tp, c_tks.get(rel.getTo()).tp, lb, ub);
            }
        }
    }

    private void expand_event(final Token tk) {
        Map<String, Token> c_tks = new HashMap<>();
        c_tks.put(THIS, tk);
        for (String id : tk.event.getEvents()) {
            Token c_tk = new Token(tk, network.newVar(), event_templates.get(id));
            tokens.add(c_tk);
            for (LessonManagerListener l : listeners) {
                l.newToken(tk);
            }
            c_tks.put(id, c_tk);
            prop_q.push(c_tk);
        }
        for (Relation rel : tk.event.getRelations()) {
            double lb = rel.getLb() != null ? rel.getUnit().convert(rel.getLb(), TimeUnit.MILLISECONDS) : Double.NEGATIVE_INFINITY;
            double ub = rel.getUb() != null ? rel.getUnit().convert(rel.getUb(), TimeUnit.MILLISECONDS) : Double.NEGATIVE_INFINITY;
            network.newTemporalRelation(c_tks.get(rel.getFrom()).tp, c_tks.get(rel.getTo()).tp, lb, ub);
        }
    }

    public void propagate() {
        while (!prop_q.isEmpty()) {
            Token tk = prop_q.pop();
            if (tk.event.getTriggerCondition() != null) {
                triggerable_tokens.add(tk);
            } else {
                expand_event(tk);
            }
        }
        // we propagate the temporal network..
        network.propagate();
    }

    public Token getToken(final int var) {
        return tokens.get(var);
    }

    public void moveToken(final Token tk, final double value) {
        network.setValue(tk.tp, value);
        // we propagate the temporal network..
        network.propagate();
    }

    @Override
    public void newValue(int var) {
        for (LessonManagerListener l : listeners) {
            l.movedToken(tokens.get(var));
        }
    }

    public void addSolverListener(LessonManagerListener listener) {
        listeners.add(listener);
    }

    public void removeSolverListener(LessonManagerListener listener) {
        listeners.remove(listener);
    }
}
