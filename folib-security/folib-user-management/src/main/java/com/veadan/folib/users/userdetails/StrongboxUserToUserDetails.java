package com.veadan.folib.users.userdetails;

import java.util.function.Function;

import com.veadan.folib.domain.User;
import org.springframework.security.core.userdetails.UserDetails;

public interface StrongboxUserToUserDetails extends Function<User, UserDetails>
{

}
