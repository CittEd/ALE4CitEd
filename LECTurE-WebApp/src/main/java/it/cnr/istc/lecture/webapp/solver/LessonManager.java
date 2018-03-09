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
package it.cnr.istc.lecture.webapp.solver;

import it.cnr.istc.lecture.api.Lesson;
import it.cnr.istc.lecture.api.model.EventTemplate;
import it.cnr.istc.lecture.api.model.LessonModel;
import it.cnr.istc.lecture.api.model.Relation;
import it.cnr.istc.lecture.webapp.time.TemporalListener;
import it.cnr.istc.lecture.webapp.time.TemporalNetwork;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.IdentityHashMap;
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
public class LessonManager implements TemporalListener {

    private static final Logger LOG = Logger.getLogger(LessonManager.class.getName());
    public static final String THIS = "this";
    private final Lesson lesson;
    private final LessonModel lesson_model;
    public final TemporalNetwork network = new TemporalNetwork(16);
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
    private AnswerContext answer_context = null;
    private final Map<SolverToken, AnswerContext> answer_contexts = new IdentityHashMap<>();
    /**
     * For each question, the corresponding answer.
     */
    private final Map<SolverToken, Integer> answers = new IdentityHashMap<>();
    private final Deque<SolverToken> prop_q = new ArrayDeque<>();
    private final List<Long> lesson_timeline_pulses = new ArrayList<>();
    private final List<Collection<SolverToken>> lesson_timeline_values = new ArrayList<>();
    private long t_now = 0;
    private int idx = 0;
    private final Collection<LessonManagerListener> listeners = new ArrayList<>();

    public LessonManager(Lesson lesson, LessonModel lesson_model) {
        this.lesson = lesson;
        this.lesson_model = lesson_model;
        network.addTemporalListener(this);
    }

    public Lesson getLesson() {
        return lesson;
    }

    public void solve() {
        for (EventTemplate event_template : lesson_model.model) {
            if (event_templates.containsKey(event_template.name)) {
                LOG.log(Level.WARNING, "Renaming event {0}", event_template.name);
            }
            event_templates.put(event_template.name, event_template);
        }

        Map<String, SolverToken> c_tks = new HashMap<>();
        // we create the tokens..
        for (String id : lesson_model.events) {
            SolverToken tk = new SolverToken(null, network.newTimePoint(), event_templates.get(id), null);
            tokens.add(tk);
            listeners.forEach(l -> l.newToken(tk));
            c_tks.put(id, tk);
            prop_q.push(tk);
        }

        // we enforce the temporal relations..
        for (Relation rel : lesson_model.relations) {
            double lb = rel.lb != null ? TimeUnit.MILLISECONDS.convert(rel.lb, rel.unit) : Double.NEGATIVE_INFINITY;
            double ub = rel.ub != null ? TimeUnit.MILLISECONDS.convert(rel.ub, rel.unit) : Double.NEGATIVE_INFINITY;
            if (rel.from.equals(THIS)) {
                network.addConstraint(0, c_tks.get(rel.to).tp, lb, ub);
            } else {
                network.addConstraint(c_tks.get(rel.from).tp, c_tks.get(rel.to).tp, lb, ub);
            }
        }

        // we build the lesson..
        build();

        // we extract the lesson timeline..
        extract_timeline();
    }

    private void expand_token(final SolverToken tk) {
        Map<String, SolverToken> c_tks = new HashMap<>();
        c_tks.put(THIS, tk);
        // we create the tokens..
        for (String id : tk.template.events) {
            SolverToken c_tk = new SolverToken(tk, network.newTimePoint(), event_templates.get(id), null);
            tokens.add(c_tk);
            listeners.forEach(l -> l.newToken(tk));
            c_tks.put(id, c_tk);
            prop_q.push(c_tk);
        }

        // we enforce the temporal relations..
        for (Relation rel : tk.template.relations) {
            double lb = rel.lb != null ? TimeUnit.MILLISECONDS.convert(rel.lb, rel.unit) : Double.NEGATIVE_INFINITY;
            double ub = rel.ub != null ? TimeUnit.MILLISECONDS.convert(rel.ub, rel.unit) : Double.NEGATIVE_INFINITY;
            network.addConstraint(c_tks.get(rel.from).tp, c_tks.get(rel.to).tp, lb, ub);
        }

        if (answer_context != null) {
            answer_context.tokens.add(tk);
        }
    }

    private void build() {
        while (!prop_q.isEmpty()) {
            SolverToken tk = prop_q.pop();
            if (tk.template.trigger_condition != null) {
                triggerable_tokens.add(tk);
            } else {
                expand_token(tk);
            }
        }
        // we propagate the temporal network..
        network.propagate();
        // we guarantee that the origin is at 0..
        network.setValue(0, 0);
    }

    private void extract_timeline() {
        lesson_timeline_pulses.clear();
        lesson_timeline_values.clear();
        Set<Long> c_pulses = new HashSet<>();
        Map<Long, Collection<SolverToken>> at = new HashMap<>();
        tokens.stream().filter(tk -> tk.enabled).forEach(tk -> {
            long pulse = (long) network.value(tk.tp);
            c_pulses.add(pulse);
            Collection<SolverToken> tks = at.get(pulse);
            if (tks == null) {
                tks = new ArrayList<>();
                at.put(pulse, tks);
            }
            tks.add(tk);
        });
        Long[] c_arr_pulses = c_pulses.toArray(new Long[c_pulses.size()]);
        Arrays.sort(c_arr_pulses);
        for (Long pulse : c_arr_pulses) {
            lesson_timeline_pulses.add(pulse);
            lesson_timeline_values.add(at.get(pulse));
        }
    }

    @Override
    public void newValue(int tp, double val) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public void boundChange(int tp, double min, double max) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public void distanceChange(int tp_from, int tp_to, double min, double max) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    private class AnswerContext {

        private final SolverToken question;
        private final int answer;
        final Collection<SolverToken> tokens = new ArrayList<>();

        private AnswerContext(SolverToken question, int answer) {
            this.question = question;
            this.answer = answer;
        }

        public SolverToken getQuestion() {
            return question;
        }

        public int getAnswer() {
            return answer;
        }

        public Collection<SolverToken> getTokens() {
            return Collections.unmodifiableCollection(tokens);
        }
    }
}
