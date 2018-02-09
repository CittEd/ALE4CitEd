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
package it.cnr.istc.ale.client;

import it.cnr.istc.ale.api.Lesson;
import it.cnr.istc.ale.api.messages.Event;
import it.cnr.istc.ale.client.context.Context;
import javafx.geometry.Insets;
import javafx.scene.control.ListView;
import javafx.scene.control.TextField;
import javafx.scene.layout.GridPane;
import static javafx.scene.layout.GridPane.setHgrow;
import javafx.scene.layout.Priority;

/**
 *
 * @author Riccardo De Benedictis
 */
public class FollowedLessonGrid extends GridPane {

    private Lesson lesson;
    private final TextField lesson_name = new TextField();
    private final ListView<Event> events = new ListView<>();

    public FollowedLessonGrid() {
        setHgap(10);
        setVgap(10);
        setPadding(new Insets(10));
        setHgrow(lesson_name, Priority.ALWAYS);
        setVgrow(events, Priority.ALWAYS);

        add(lesson_name, 0, 0, 2, 1);
        add(events, 0, 1);
    }

    public void setLesson(final Lesson lesson) {
        this.lesson = lesson;
        lesson_name.setText(lesson.getName());
        events.setItems(Context.getContext().getLearningContext().getEvents(lesson));
    }
}
