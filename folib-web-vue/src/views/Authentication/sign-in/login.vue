<!--
	This is the illustration sign up page, it uses the dashboard layout in:
	"./layouts/Default.vue" .
 -->

<template>
	<div class="sign-in">

		<a-row type="flex" :gutter="[24, 24]" justify="space-around" align="middle" class="row-main">

			<!-- Sign in Form Column -->
			<a-col :span="24" :md="{ span: 14, offset: 2 }" :lg="10" :xl="6" class="col-form mr-auto">

				<h4 class="mb-15">{{ $t('Authentication.Login') }}</h4>
				<p class="text-muted">{{ $t('Authentication.UsingUsernamePasswordLogin') }}</p>

				<!-- <a-tabs
					:default-active-key="1"
					:activeKey="loginTypeActiveKey"
					@change="loginTypeChange($event)"
				>
					<a-tab-pane :key="1" :tab="$t('Authentication.LocalAuthentication')"> -->
							<!-- Sign in Form -->
						<a-form id="components-form-demo-normal-login" :form="form" class="login-form" @submit="handleSubmit"
							:hideRequiredMark="true">
							<a-form-item class="mb-10" :label="$t('Authentication.Username')" :colon="false">
								<a-input v-decorator="[
									'username',
									{ rules: [{ required: true, message: $t('Authentication.EnterYourUsername') }] },
								]" :placeholder="$t('Authentication.EnterYourUsername')" />
							</a-form-item>
							<a-form-item class="mb-5" :label="$t('Authentication.Password')" :colon="false">
								<a-input-password v-decorator="[
									'password',
									{ rules: [{ required: true, message: $t('Authentication.EnterThePassword')}] },
								]" type="password" :placeholder="$t('Authentication.EnterThePassword')" />
							</a-form-item>
							<!-- <a-form-item class="mb-10">
								<a-checkbox v-decorator="[
									'remember',
									{
										rules: [{ required: true, message: '请勾选同意本协议'},{validator: checkRemember }],
										valuePropName: 'checked',
										initialValue: true,
									},
								]">
									保存密码 <a href="#" class="font-bold text-dark">同意本协议</a>
								</a-checkbox>
							</a-form-item> -->
							<a-form-item>
								<a-button type="primary" block html-type="submit" class="login-form-button">
                  {{ $t('Authentication.Login') }}
								</a-button>
							</a-form-item>
						</a-form>
					<!-- / Sign Up Form -->
					<!-- </a-tab-pane> -->
					<!-- <a-tab-pane :key="2" :tab="'LDAP' + $t('Authentication.Authentication')">
						<a-form id="components-form-demo-normal-login" :form="form" class="login-form" @submit="handleSubmit"
							:hideRequiredMark="true">
							<a-form-item class="mb-10" :label="'LDAP' + $t('Authentication.Username')" :colon="false">
								<a-input v-decorator="[
									'username',
									{ rules: [{ required: true, message: $t('Authentication.enterLDAPUsername') }] },
								]" :placeholder="$t('Authentication.enterLDAPUsername')" />
							</a-form-item>
							<a-form-item class="mb-5" :label="$t('Authentication.Password')" :colon="false">
								<a-input v-decorator="[
									'password',
									{ rules: [{ required: true, message: $t('Authentication.EnterThePassword')}] },
								]" type="password" :placeholder="$t('Authentication.EnterThePassword')" />
							</a-form-item>
							<a-form-item>
								<a-button type="primary" block html-type="submit" class="login-form-button">
                  {{ $t('Authentication.Login') }}
								</a-button>
							</a-form-item>
						</a-form>
					</a-tab-pane> -->
				</a-tabs>
        <Clients :clients="clientList"></Clients>
				<!-- <p class="font-semibold text-muted text-center">没有账号? <router-link to="/sign-in"
						class="font-bold text-dark">注册</router-link>
				</p> -->
			</a-col>
			<!-- / Sign Up Form Column -->

			<!-- Sign Up Image Column -->
			<a-col :span="24" :md="12" :lg="12" :xl="12" class="col-img">
				<div>
					<div class="img">
						<img src="images/login-bk.svg" alt="rocket">
					</div>
					<h5 class="text-white">{{ instanceName }} 携手国产化，引领信创未来，我们为你的软件制品保驾护航！</h5>
					<p class="text-white">{{ instanceName }} Joining Hands in National Innovation, Pioneering the Future of Technology, We Safeguard Your Software Solutions!</p>
				</div>
			</a-col>
			<!-- / Sign Up Image Column -->

		</a-row>


	</div>
</template>

<script>
import store from '@/store'
import { encrypt } from "@/utils/jsencrypt"
import {
  getSsoList,
} from '@/api/sso'
import {
  getConfig,
} from '@/api/foEyes'
import {checkMachineCode,getServerName} from "@/api/settings"
import  Clients  from "../../../components/loginClients/clients";
export default {
  components:{
    Clients
  },
	data() {
		return {
      instanceName:sessionStorage.getItem("instanceName")||"",
      clientList:[],
			// Sign up form object.
			form: this.$form.createForm(this, { name: 'signup_illustration' }),
			loginTypeActiveKey: 1,
		}
	},
  created(){
    getSsoList().then(res=>{
      this.clientList=res
      console.log(this.clientList);
    })
  },

	methods: {
		loginTypeChange(activeKey) {
			this.form.resetFields()
      this.loginTypeActiveKey = activeKey
    },
		// Handles input validation after submission.
		handleSubmit(e) {
			e.preventDefault()
			this.form.validateFields((err, values) => {
				if (!err) {
					let password = encrypt(values.password)
					let data = {username: values.username, password: password}
					if (this.loginTypeActiveKey === 2) {
						data.headers = {"X-Folibrary-Login-Type": "LDAP"}
					}
					store.dispatch("Login", data).then((res) => {
						if (res.token != null) {
							store.dispatch("GetInfo").then((res) => {
							})
							getConfig().then((res) => {
								localStorage.setItem("FOEYES_CONFIG", JSON.stringify(res))
							})
							sessionStorage.setItem("identityLevel",'basic')
							checkMachineCode().then(res=>{
								sessionStorage.setItem("identityLevel",res.level)
							})
						}
						// 延迟显示欢迎信息
						setTimeout(() => {
							this.$router.push({ name: 'storagesHome' })
							this.$notification.success({
								message: this.$t('Authentication.Welcome'),
							})
						}, 100)
					})
				}
			});
		},
		checkRemember (rule, value, callback) {
			if (value === false) {
				callback(new Error(this.$t('Authentication.CheckAgreementProceed')))
			} else {
				callback()
			}
		}
	},
}

</script>

<style lang="scss">

</style>
