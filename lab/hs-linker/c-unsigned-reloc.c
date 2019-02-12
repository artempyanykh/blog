static int answer = 42;
static int * pAnswer = &answer;

int getAnswer(void)
{
        return *pAnswer;
}
