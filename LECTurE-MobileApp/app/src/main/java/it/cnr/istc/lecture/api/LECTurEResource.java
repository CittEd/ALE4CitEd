package it.cnr.istc.lecture.api;

import java.util.Collection;

import retrofit2.Call;
import retrofit2.http.Body;
import retrofit2.http.DELETE;
import retrofit2.http.Field;
import retrofit2.http.FormUrlEncoded;
import retrofit2.http.GET;
import retrofit2.http.POST;
import retrofit2.http.PUT;
import retrofit2.http.Path;

public interface LECTurEResource {

    @POST("LECTurE-WebApp-1.0/LECTurE/new_user")
    User newUser(@Body NewUserRequest new_user);

    @GET("LECTurE-WebApp-1.0/LECTurE/users")
    Call<Collection<User>> getUsers();

    @GET("LECTurE-WebApp-1.0/LECTurE/find_users/{search_string}")
    Collection<User> findUsers(@Path("search_string") String search_string);

    @GET("LECTurE-WebApp-1.0/LECTurE/users/{user_id}")
    User getUser(@Path("user_id") long user_id);

    @DELETE("LECTurE-WebApp-1.0/LECTurE/users/{user_id}")
    void deleteUser(@Path("user_id") long user_id);

    @FormUrlEncoded
    @PUT("LECTurE-WebApp-1.0/LECTurE/add_teacher")
    void addTeacher(@Field("student_id") long student_id, @Field("teacher_id") long teacher_id);

    @FormUrlEncoded
    @PUT("LECTurE-WebApp-1.0/LECTurE/remove_teacher")
    void removeTeacher(@Field("student_id") long student_id, @Field("teacher_id") long teacher_id);

    @POST("LECTurE-WebApp-1.0/LECTurE/login")
    Call<InitResponse> login(@Body Credentials credentials);

    @FormUrlEncoded
    @PUT("LECTurE-WebApp-1.0/LECTurE/answer_question")
    void answerQuestion(@Field("lesson_id") long lesson_id, @Field("question_id") int question_id, @Field("answer_id") int answer_id);

    @POST("LECTurE-WebApp-1.0/LECTurE/new_lesson_by_model")
    Lesson newLessonByModel(@Body NewLessonRequest new_lesson);

    @POST("LECTurE-WebApp-1.0/LECTurE/new_lesson_by_model_id")
    Lesson newLessonByModelId(@Body NewLessonRequest new_lesson);

    @GET("LECTurE-WebApp-1.0/LECTurE/lessons")
    Collection<Lesson> getLessons();

    @FormUrlEncoded
    @PUT("LECTurE-WebApp-1.0/LECTurE/solve_lesson")
    void solveLesson(@Field("lesson_id") long lesson_id);

    @FormUrlEncoded
    @PUT("LECTurE-WebApp-1.0/LECTurE/play")
    void play(@Field("lesson_id") long lesson_id);

    @FormUrlEncoded
    @PUT("LECTurE-WebApp-1.0/LECTurE/pause")
    void pause(@Field("lesson_id") long lesson_id);

    @FormUrlEncoded
    @PUT("LECTurE-WebApp-1.0/LECTurE/stop")
    void stop(@Field("lesson_id") long lesson_id);

    @FormUrlEncoded
    @PUT("LECTurE-WebApp-1.0/LECTurE/go_to")
    void goTo(@Field("lesson_id") long lesson_id, @Field("time") long time);

    @FormUrlEncoded
    @PUT("LECTurE-WebApp-1.0/LECTurE/set_time")
    void setTime(@Field("lesson_id") long lesson_id, @Field("token_id") int token_id, @Field("time") long time);

    @DELETE("LECTurE-WebApp-1.0/LECTurE/lessons/{lesson_id}")
    void deleteLesson(@Path("lesson_id") long lesson_id);
}
