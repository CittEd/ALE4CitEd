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
package it.cnr.istc.ale.client.context;

import it.cnr.istc.ale.api.Lesson;
import it.cnr.istc.ale.api.LessonState;
import it.cnr.istc.ale.api.model.LessonModel;
import it.cnr.istc.ale.client.context.TeachingContext.TokenRow;
import java.util.HashMap;
import java.util.Map;
import javafx.beans.Observable;
import javafx.beans.property.LongProperty;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleLongProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;

/**
 *
 * @author Riccardo De Benedictis
 */
public class LessonContext {

    final Lesson lesson;
    final LessonModel model;
    final ObjectProperty<LessonState> state = new SimpleObjectProperty<>();
    final LongProperty time = new SimpleLongProperty(0);
    final ObservableList<TokenRow> tokens = FXCollections.observableArrayList();
    private final Map<Integer, TokenRow> id_tokens = new HashMap<>();

    LessonContext(Lesson lesson, LessonModel model) {
        this.lesson = lesson;
        this.model = model;
        tokens.addListener((ListChangeListener.Change<? extends TokenRow> c) -> {
            while (c.next()) {
                c.getAddedSubList().forEach((tk) -> id_tokens.put(tk.getId(), tk));
                c.getRemoved().forEach((tk) -> id_tokens.remove(tk.getId()));
            }
        });
    }

    public Lesson getLesson() {
        return lesson;
    }

    public LessonModel getModel() {
        return model;
    }

    public ObjectProperty<LessonState> getState() {
        return state;
    }

    public LongProperty getTime() {
        return time;
    }

    public ObservableList<TokenRow> getTokens() {
        return tokens;
    }

    public TokenRow getToken(final int id) {
        return id_tokens.get(id);
    }
}
