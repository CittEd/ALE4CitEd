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
import it.cnr.istc.ale.client.context.Context;
import it.cnr.istc.ale.client.context.TeachingContext;
import java.util.IdentityHashMap;
import java.util.Map;
import javafx.animation.FadeTransition;
import javafx.animation.Interpolator;
import javafx.animation.KeyFrame;
import javafx.animation.KeyValue;
import javafx.animation.Timeline;
import javafx.beans.property.LongProperty;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.effect.Bloom;
import javafx.scene.effect.DropShadow;
import javafx.scene.layout.Pane;
import javafx.scene.paint.Color;
import javafx.scene.shape.Circle;
import javafx.scene.shape.Line;
import javafx.util.Duration;

/**
 *
 * @author Riccardo De Benedictis
 */
public class LessonTimeline extends Pane {

    private static final int BAR_POSITION = 300;
    private Lesson lesson;
    private final Map<TeachingContext.TokenRow, TokenCircle> tokens = new IdentityHashMap<>();

    public LessonTimeline() {
        prefHeight(30);
        setStyle("-fx-background-color: #FFFFFF;");
    }

    public void setLesson(final Lesson lesson) {
        this.lesson = lesson;
        getChildren().clear();
        LongProperty lessonTime = Context.getContext().getTeachingContext().getLessonTime(lesson);
        ObservableList<TeachingContext.TokenRow> tks = Context.getContext().getTeachingContext().getTokens(lesson);

        Line line = new Line(BAR_POSITION, 0, BAR_POSITION, 30);
        DropShadow ds = new DropShadow();
        ds.setOffsetX(2);
        ds.setOffsetY(2);
        ds.setColor(Color.GRAY);
        line.setEffect(ds);
        getChildren().add(line);

        for (TeachingContext.TokenRow tk : tks) {
            TokenCircle tk_circle = new TokenCircle(tk, lessonTime);
            tokens.put(tk, tk_circle);
            getChildren().add(tk_circle);
        }
        tks.addListener((ListChangeListener.Change<? extends TeachingContext.TokenRow> c) -> {
            while (c.next()) {
                for (TeachingContext.TokenRow tk : c.getAddedSubList()) {
                    TokenCircle tk_circle = new TokenCircle(tk, lessonTime);
                    tokens.put(tk, tk_circle);
                    getChildren().add(tk_circle);
                    FadeTransition f_t = new FadeTransition(Duration.millis(1000), tk_circle);
                    f_t.setFromValue(0);
                    f_t.setToValue(1);
                    f_t.play();
                }
                for (TeachingContext.TokenRow tk : c.getRemoved()) {
                    TokenCircle tk_circle = tokens.remove(tk);
                    FadeTransition f_t = new FadeTransition(Duration.millis(1000), tk_circle);
                    f_t.setFromValue(1);
                    f_t.setToValue(0);
                    f_t.play();
                    f_t.onFinishedProperty().addListener((ObservableValue<? extends EventHandler<ActionEvent>> observable, EventHandler<ActionEvent> oldValue, EventHandler<ActionEvent> newValue) -> {
                        tk_circle.unbind();
                        getChildren().remove(tk_circle);
                    });
                }
            }
        });
    }

    private class TokenCircle extends Circle {

        private final TeachingContext.TokenRow tk;
        private final LongProperty lessonTime;
        private final TimeChangeListener time_change_listener = new TimeChangeListener();
        private final LessonTimeChangeListener lesson_time_change_listener = new LessonTimeChangeListener();

        private TokenCircle(TeachingContext.TokenRow tk, LongProperty lessonTime) {
            super(6, Color.DARKBLUE);
            this.tk = tk;
            centerYProperty().set(17);
            centerXProperty().set(tk.getTime() / 100 + BAR_POSITION);
            tk.timeProperty().addListener(time_change_listener);
            lessonTime.addListener(lesson_time_change_listener);
            this.lessonTime = lessonTime;

            DropShadow ds = new DropShadow();
            ds.setOffsetX(2);
            ds.setOffsetY(2);
            ds.setColor(Color.GRAY);
            ds.setInput(new Bloom(0.7));

            setEffect(ds);
        }

        private void unbind() {
            tk.timeProperty().removeListener(time_change_listener);
            lessonTime.removeListener(lesson_time_change_listener);
        }

        private class TimeChangeListener implements ChangeListener<Number> {

            @Override
            public void changed(ObservableValue<? extends Number> observable, Number oldValue, Number newValue) {
                final Timeline tl = new Timeline();
                final KeyValue kv = new KeyValue(centerXProperty(), newValue.doubleValue() / 100 + BAR_POSITION, Interpolator.EASE_BOTH);
                final KeyFrame kf = new KeyFrame(Duration.millis(500), kv);
                tl.getKeyFrames().add(kf);
                tl.play();
            }
        }

        private class LessonTimeChangeListener implements ChangeListener<Number> {

            @Override
            public void changed(ObservableValue<? extends Number> observable, Number oldValue, Number newValue) {
                final Timeline tl = new Timeline();
                final KeyValue kv = new KeyValue(translateXProperty(), -newValue.doubleValue() / 100, Interpolator.EASE_BOTH);
                final KeyFrame kf = new KeyFrame(Duration.millis(500), kv);
                tl.getKeyFrames().add(kf);
                tl.play();
            }
        }
    }
}
