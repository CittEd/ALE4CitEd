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

import it.cnr.istc.ale.api.model.EventTemplate;
import it.cnr.istc.ale.api.model.LessonModel;
import it.cnr.istc.ale.api.model.Relation;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Riccardo De Benedictis
 */
public class LessonManager implements TemporalNetworkListener {

    public static final String THIS = "this";
    private static final Logger LOG = Logger.getLogger(LessonManager.class.getName());
    public final TemporalNetwork network = new TemporalNetwork();
    private final Map<String, EventTemplate> event_templates = new HashMap<>();
    /**
     * For each time point, the corresponding token.
     */
    private final List<SolverToken> tokens = new ArrayList<>();
    /**
     * A collection of triggerable tokens. These tokens are expanded whenever
     * their triggering condition becomes satisfied.
     */
    private final Collection<SolverToken> triggerable_tokens = new ArrayList<>();
    private final Deque<SolverToken> prop_q = new ArrayDeque<>();
    public final int origin = network.newVar();
    private final List<Long> lesson_timeline_pulses = new ArrayList<>();
    private final List<Collection<SolverToken>> lesson_timeline_values = new ArrayList<>();
    private long t_now = 0;
    private int idx = 0;
    private final Collection<LessonManagerListener> listeners = new ArrayList<>();

    public LessonManager() {
        network.addTemporalNetworkListener(this);
    }

    public void setModel(final LessonModel model) {
        for (EventTemplate event_template : model.getModel()) {
            if (event_templates.containsKey(event_template.getName())) {
                LOG.log(Level.WARNING, "Renaming event {0}", event_template.getName());
            }
            event_templates.put(event_template.getName(), event_template);
        }

        Map<String, SolverToken> c_tks = new HashMap<>();
        // we create the tokens..
        for (String id : model.getEvents()) {
            SolverToken tk = new SolverToken(null, network.newVar(), event_templates.get(id));
            tokens.add(tk);
            listeners.forEach(l -> l.newToken(tk));
            c_tks.put(id, tk);
            prop_q.push(tk);
        }

        // we enforce the temporal relations..
        for (Relation rel : model.getRelations()) {
            double lb = rel.getLb() != null ? TimeUnit.MILLISECONDS.convert(rel.getLb(), rel.getUnit()) : Double.NEGATIVE_INFINITY;
            double ub = rel.getUb() != null ? TimeUnit.MILLISECONDS.convert(rel.getUb(), rel.getUnit()) : Double.NEGATIVE_INFINITY;
            if (rel.getFrom().equals(THIS)) {
                network.newTemporalRelation(origin, c_tks.get(rel.getTo()).tp, lb, ub);
            } else {
                network.newTemporalRelation(c_tks.get(rel.getFrom()).tp, c_tks.get(rel.getTo()).tp, lb, ub);
            }
        }

        // we build the lesson..
        build();
    }

    private void extract_timeline() {
        lesson_timeline_pulses.clear();
        lesson_timeline_values.clear();
        Set<Long> c_pulses = new HashSet<>();
        Map<Long, Collection<SolverToken>> at = new HashMap<>();
        for (SolverToken tk : tokens) {
            long pulse = (long) network.getValue(tk.tp);
            c_pulses.add(pulse);
            Collection<SolverToken> tks = at.get(pulse);
            if (tks == null) {
                tks = new ArrayList<>();
                at.put(pulse, tks);
            }
            tks.add(tk);
        }
        Long[] c_arr_pulses = c_pulses.toArray(new Long[c_pulses.size()]);
        Arrays.sort(c_arr_pulses);
        for (Long pulse : c_arr_pulses) {
            lesson_timeline_pulses.add(pulse);
            lesson_timeline_values.add(at.get(pulse));
        }
    }

    private void expand_event(final SolverToken tk) {
        Map<String, SolverToken> c_tks = new HashMap<>();
        c_tks.put(THIS, tk);
        // we create the tokens..
        for (String id : tk.template.getEvents()) {
            SolverToken c_tk = new SolverToken(tk, network.newVar(), event_templates.get(id));
            tokens.add(c_tk);
            listeners.forEach(l -> l.newToken(tk));
            c_tks.put(id, c_tk);
            prop_q.push(c_tk);
        }

        // we enforce the temporal relations..
        for (Relation rel : tk.template.getRelations()) {
            double lb = rel.getLb() != null ? TimeUnit.MILLISECONDS.convert(rel.getLb(), rel.getUnit()) : Double.NEGATIVE_INFINITY;
            double ub = rel.getUb() != null ? TimeUnit.MILLISECONDS.convert(rel.getUb(), rel.getUnit()) : Double.NEGATIVE_INFINITY;
            network.newTemporalRelation(c_tks.get(rel.getFrom()).tp, c_tks.get(rel.getTo()).tp, lb, ub);
        }
    }

    private void build() {
        while (!prop_q.isEmpty()) {
            SolverToken tk = prop_q.pop();
            if (tk.template.getTriggerCondition() != null) {
                triggerable_tokens.add(tk);
            } else {
                expand_event(tk);
            }
        }
        // we propagate the temporal network..
        network.propagate();
        // we guarantee that the origin is at 0..
        network.setValue(origin, 0);

        // we extract the lesson timeline..
        extract_timeline();
    }

    public long getCurrentTime() {
        return t_now;
    }

    public SolverToken getToken(final int var) {
        return tokens.get(var - 1);
    }

    public void setTime(final int var, final double value) {
        network.setValue(var, value);

        // we extract the lesson timeline..
        extract_timeline();
    }

    public List<SolverToken> getTokens() {
        return Collections.unmodifiableList(tokens);
    }

    public List<SolverToken> getTokensUpToNow() {
        List<SolverToken> tks = new ArrayList<>();
        for (int i = 0; i < lesson_timeline_pulses.size() && t_now >= lesson_timeline_pulses.get(i); i++) {
            tks.addAll(lesson_timeline_values.get(i));
        }
        return tks;
    }

    public void moveToken(final SolverToken tk, final double value) {
        network.setValue(tk.tp, value);
        // we propagate the temporal network..
        network.propagate();
        // we guarantee that the origin is at 0..
        network.setValue(origin, 0);

        // we extract the lesson timeline..
        extract_timeline();
    }

    @Override
    public void newValue(int var) {
        if (var != origin) {
            listeners.forEach(l -> l.movedToken(getToken(var)));
        }
    }

    /**
     * Executes one tick. In other words moves the execution of the lesson
     * forward of one second.
     */
    public void tick() {
        goTo(t_now + 1000);
    }

    /**
     * Executes the lesson, either forward or backward, till the given relative
     * (to the lesson) time.
     *
     * @param t the relative current time.
     */
    public void goTo(final long t) {
        if (t > t_now && idx < lesson_timeline_pulses.size()) {
            // we are moving forward..
            long next_pulse = lesson_timeline_pulses.get(idx);
            while (next_pulse <= t) {
                lesson_timeline_values.get(idx).forEach(tk -> listeners.forEach(l -> l.executeToken(tk)));
                idx++;
                if (idx < lesson_timeline_pulses.size()) {
                    next_pulse = lesson_timeline_pulses.get(idx);
                } else {
                    // we have no more tokens to execute..
                    break;
                }
            }
        }
        if (t < t_now && idx > 0) {
            // we are moving backward..
            long last_pulse = lesson_timeline_pulses.get(idx - 1);
            while (last_pulse > t) {
                lesson_timeline_values.get(idx - 1).forEach(tk -> listeners.forEach(l -> l.hideToken(tk)));
                idx--;
                if (idx > 0) {
                    last_pulse = lesson_timeline_pulses.get(idx - 1);
                } else {
                    // we have no more tokens to hide..
                    break;
                }
            }
        }
        t_now = t;
        listeners.forEach(l -> l.newTime(t_now));
    }

    public void addSolverListener(LessonManagerListener listener) {
        listeners.add(listener);
    }

    public void removeSolverListener(LessonManagerListener listener) {
        listeners.remove(listener);
    }
}
