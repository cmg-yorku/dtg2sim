import contextlib
import io
from src.GMEnv import GMEnv
from src.QueryEngine import QueryEngine
from src.dtg2sim import dtg2sim


class TestEngine():
    verbose = False
    dtg = False

    def __init__(self, verbose = False, dtg = False):
        self.verbose = verbose
        self.dtg = dtg

    def _color_text(self, text, color = "green"):
        colors = {
            "black": "\033[30m",
            "red": "\033[91m",
            "green": "\033[92m",
            "yellow": "\033[93m",
            "blue": "\033[94m",
            "magenta": "\033[95m",
            "cyan": "\033[96m",
            "white": "\033[97m",
            "reset": "\033[0m",
        }
        color_code = colors.get(color.lower(), colors["reset"])
        return f"{color_code}{text}{colors['reset']}"

    def exec_domain(self, filename, exp_sim = None,
                    exp_learn = None,
                    exp_dtg = None,
                    test_sim = True,
                    test_learn = True,
                    test_dtg = False):

        print(self._color_text(f"\n\n *** NOW TESTING: {filename} *** \n","yellow"))

        self.e = QueryEngine(filename)
        self.g = GMEnv(qEng=self.e)
        self.c = dtg2sim(self.g)

        sim_outcome = ""
        train_outcome = ""
        dtg_outcome = ""

        #
        # Simulation testing
        #
        print(self._color_text(f"Testing simulation...", "yellow"), end='')
        if test_sim:
            print()
            if not self.verbose:
                with contextlib.redirect_stdout(io.StringIO()):
                    obs_sim = self.c.simulate()
            else:
                obs_sim = self.c.simulate()

            sim_outcome = f"sim {obs_sim: .4f}({exp_sim})"

            try:
                assert exp_sim[0] <= obs_sim <= exp_sim[1],f"\nSpec: {filename}\n .... Observed sim value         : {obs_sim}\n .... Expected sim value interval: {exp_sim}"
            except AssertionError as e:
                print(f"Test failed: {e}")
                exit(1)  # or return / raise SystemExit


        else:
            print(self._color_text(f"skipped", "yellow"))

        #
        # Training testing
        #
        print(self._color_text(f"Testing training...", "yellow"),end = '')
        if test_learn:
            print()
            if not self.verbose:
                with contextlib.redirect_stdout(io.StringIO()):
                    _, obs_learn = self.c.learn()
            else:
                _, obs_learn = self.c.learn()

            if obs_learn == 'N/A':
                train_outcome = f"training {obs_learn}({exp_learn})"
            else:
                train_outcome = f"training {float(obs_learn): .4f}({exp_learn})"

            try:
                assert (obs_learn == "N/A") or (exp_learn[0] <= float(obs_learn) <= exp_learn[1]) , f"\nSpec: {filename}\n .... Observed learn value         : {obs_learn}\n .... Expected learn value interval: {exp_learn}"
            except AssertionError as e:
                print(f"Test failed: {e}")
                exit(1)  # or return / raise SystemExit

        else:
            print(self._color_text(f"skipped", "yellow"))


        #
        # DT-Golog
        #
        print(self._color_text(f"Testing dtg reasoning...", "yellow"),end = '')
        if test_dtg:
            print()
            if not self.verbose:
                with contextlib.redirect_stdout(io.StringIO()):
                    dtg_calc, _, _ = self.c.getDTG()
            else:
                dtg_calc, _ , _ = self.c.getDTG()

            dtg_outcome = f"dtg {dtg_calc: .4f}({exp_dtg})"

            try:
                assert exp_dtg[0] <= dtg_calc <= exp_dtg[1],f"\nSpec: {filename}\n .... Observed dtg value         : {obs_sim}\n .... Expected dtg value interval: {exp_sim}"
            except AssertionError as e:
                print(f"Test failed: {e}")
                exit(1)  # or return / raise SystemExit

        else:
            print(self._color_text(f"skipped", "yellow"))

        # If you reached this point then:
        print(self._color_text(f"pass", "green"))
        print(self._color_text(f"Avg. returns: {sim_outcome}, {train_outcome},{dtg_outcome}","green"))
        exit(0)



