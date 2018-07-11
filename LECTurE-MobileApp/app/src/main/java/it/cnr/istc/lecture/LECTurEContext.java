package it.cnr.istc.lecture;

import it.cnr.istc.lecture.api.LECTurEResource;
import retrofit2.Retrofit;
import retrofit2.converter.gson.GsonConverterFactory;

public class LECTurEContext {

    private static LECTurEContext instance;

    public static LECTurEContext getInstance() {
        if (instance == null) instance = new LECTurEContext();
        return instance;
    }

    private LECTurEResource resource;

    private LECTurEContext() {
        Retrofit retrofit = new Retrofit.Builder().baseUrl(BuildConfig.LECTURE_HOST + ":" + BuildConfig.SERVICE_PORT).addConverterFactory(GsonConverterFactory.create()).build();
        resource = retrofit.create(LECTurEResource.class);
    }

    public LECTurEResource getResource() {
        return resource;
    }
}
