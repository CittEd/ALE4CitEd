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
package it.cnr.istc.ale.server.solver;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

/**
 *
 * @author Riccardo De Benedictis
 */
public class AnswerContext {

    private final SolverToken question;
    private final int answer;
    final Collection<SolverToken> tokens = new ArrayList<>();

    AnswerContext(SolverToken question, int answer) {
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
