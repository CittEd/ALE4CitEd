package it.cnr.istc.lecture;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import it.cnr.istc.lecture.api.Lesson;
import it.cnr.istc.lecture.api.Lesson.LessonState;
import it.cnr.istc.lecture.api.messages.Token;
import it.cnr.istc.lecture.api.model.LessonModel;

public class TeachingLessonContext {

    private final Lesson lesson;
    private final LessonModel model;
    private LessonState state = LessonState.Stopped;
    private long time = 0;
    private final List<TokenRow> tokens = new ArrayList<>();
    private final Map<Integer, TokenRow> id_tokens = new HashMap<>();
    private final List<TeachingLessonListener> listeners = new ArrayList<>();

    TeachingLessonContext(Lesson lesson, LessonModel model) {
        this.lesson = lesson;
        this.model = model;
        if (lesson.tokens != null) {
            for (Token tk : lesson.tokens) {
                tokens.add(new TokenRow(tk));
            }
        }
    }

    public Lesson getLesson() {
        return lesson;
    }

    public LessonModel getModel() {
        return model;
    }

    public LessonState getState() {
        return state;
    }

    public void setState(LessonState state) {
        if (this.state != state) {
            this.state = state;
            for (TeachingLessonListener l : listeners) l.stateChanged(state);
        }
    }

    public long getTime() {
        return time;
    }

    public void setTime(long time) {
        if (this.time != time) {
            this.time = time;
            for (TeachingLessonListener l : listeners) l.timeChanged(time);
        }
    }

    public List<TokenRow> getTokens() {
        return Collections.unmodifiableList(tokens);
    }

    public void addToken(final Token tk) {
        TokenRow tk_r = new TokenRow(tk);
        tokens.add(tk_r);
        id_tokens.put(tk.id, tk_r);
        for (TeachingLessonListener l : listeners) l.newToken(tk_r);
    }

    public void removeToken(final Token tk) {
        TokenRow tk_r = id_tokens.remove(tk.id);
        tokens.remove(tk_r);
        for (TeachingLessonListener l : listeners) l.removedToken(tk_r);
    }

    public void addListener(TeachingLessonListener l) {
        listeners.add(l);
    }

    public void removeListener(TeachingLessonListener l) {
        listeners.remove(l);
    }

    public static class TokenRow {

        private final Token tk;

        private TokenRow(Token tk) {
            this.tk = tk;
        }
    }

    public interface TeachingLessonListener {

        void timeChanged(long t);

        void stateChanged(LessonState state);

        void newToken(TokenRow tk);

        void removedToken(TokenRow tk);
    }
}
