<!-- 
	This is the illustration sign up page, it uses the dashboard layout in: 
	"./layouts/Default.vue" .
 -->

<template>
	<div class="sign-in">
		
		<a-row type="flex" :gutter="[24,24]" justify="space-around" align="middle" class="row-main">

			<!-- Sign in Form Column -->
			<a-col :span="24" :md="{ span: 14, offset: 2 }" :lg="10" :xl="6" class="col-form mr-auto">

				<h4 class="mb-15">登录</h4>
				<p class="text-muted">使用用户名和密码进行登录操作</p>

				<!-- Sign in Form -->
				<a-form
					id="components-form-demo-normal-login"
					:form="form"
					class="login-form"
					@submit="handleSubmit"
					:hideRequiredMark="true"
				>
					<a-form-item class="mb-10" label="用户名" :colon="false">
						<a-input 
						v-decorator="[
						'username',
						{ rules: [{ required: true, message: '请输入用户名!' }] },
						]" placeholder="Name" />
					</a-form-item>
					<a-form-item class="mb-5" label="密码" :colon="false">
						<a-input
						v-decorator="[
						'password',
						{ rules: [{ required: true, message: '请输入密码!' }] },
						]" type="password" placeholder="Password" />
					</a-form-item>
					<a-form-item class="mb-10">
						<a-checkbox
							v-decorator="[
							'remember',
							{
								valuePropName: 'checked',
								initialValue: true,
							},
							]"
						>
							保存密码 <a href="#" class="font-bold text-dark">同意本协议</a>
						</a-checkbox>
					</a-form-item>
					<a-form-item>
						<a-button type="primary" block html-type="submit"  class="login-form-button">
							登录
						</a-button>
					</a-form-item>
				</a-form>
				<!-- / Sign Up Form -->

			<p class="font-semibold text-muted text-center">没有账号? <router-link to="/sign-in" class="font-bold text-dark">注册</router-link></p>
			</a-col>
			<!-- / Sign Up Form Column -->

			<!-- Sign Up Image Column -->
			<a-col :span="24" :md="12" :lg="12" :xl="12" class="col-img">
				<div>
					<div class="img">
						<img src="images/info-rocket-ill.png" alt="rocket">
					</div>
					<h4 class="text-white">Library为你的软件制品保驾护航,为国产化、信创加油助力!</h4>
					<p class="text-white">Library starts from FO" Buddha "to protect your products!</p>
				</div>
			</a-col>
			<!-- / Sign Up Image Column -->

		</a-row>
		
	</div>
</template>

<script>
import store from '@/store'
	export default ({
		data() {
			return {
				// Sign up form object.
				form: this.$form.createForm(this, { name: 'signup_illustration' }),
			}
		},
		methods: {
			// Handles input validation after submission.
			handleSubmit(e) {
				e.preventDefault();
				this.form.validateFields((err, values) => {

					if ( !err ) {
            store.dispatch("Login",values).then((res) => {
              if(res.token!=null){
                store.dispatch("GetInfo").then((res) => {
                })
              }
              //
               this.$router.push({ name: 'storages' })
              // 延迟 1 秒显示欢迎信息
              setTimeout(() => {
                this.$notification.success({
                  message: '欢迎',
                })
              }, 100)
            })
					}
				});
			},
		},
	})

</script>

<style lang="scss">
</style>
